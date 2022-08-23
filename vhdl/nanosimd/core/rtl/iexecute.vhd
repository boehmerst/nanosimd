library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.nanosimd_pkg.all;
use work.inst_pkg.all;
use work.func_pkg.all;

entity iexecute is
  generic (
    exec_data_width_g : integer := 32;
    exec_addr_width_g : integer := 32;
    exec_gprf_size_g  : integer := 32;
    use_m_ext_g       : boolean := false
  );
  port (
    clk_i     : in  std_ulogic;
    reset_n_i : in  std_ulogic;
    init_i    : in  std_ulogic;
    en_i      : in  std_ulogic;
    execute_i : in  execute_in_t;
    execute_o : out execute_out_t
  );
end entity iexecute;

architecture rtl of iexecute is
  ------------------------------------------------------------------------------
  -- TODO: better move to function package
  -- multiply (unsigned * unsigned, signed * signed, signed * unsigned)
  ------------------------------------------------------------------------------
  function multiply(a, b : std_ulogic_vector; mode : mul_op_t) return std_ulogic_vector is
    variable mul_opa        : unsigned(a'length-1 downto 0);
    variable mul_opb        : unsigned(b'length-1 downto 0);
    variable mul_result     : unsigned(mul_opa'length + mul_opb'length-1 downto 0);
    variable mul_neg_result : unsigned(mul_opa'length + mul_opb'length-1 downto 0);
    variable result         : unsigned(mul_opa'length + mul_opb'length-1 downto 0);
  begin

    case(mode) is
      when MUL_MULU => mul_opa := unsigned(a);
                       mul_opb := unsigned(b);

      when MUL_MULS => if(a(a'left) = '1') then
                         mul_opa := unsigned(not a) + 1;
                       else
                         mul_opa := unsigned(a);
                       end if;

                       if(b(b'left) = '1') then
                         mul_opb := unsigned(not b) + 1;
                       else
                         mul_opb := unsigned(b);
                       end if;

      when others   => if(a(a'left) = '1') then
                         mul_opa := unsigned(not a) + 1;
                       else
                         mul_opa := unsigned(a);
                       end if;
                       mul_opb   := unsigned(b);
    end case;

    mul_result     := mul_opa * mul_opb;
    mul_neg_result := (not mul_result) + 1; 

    case(mode) is
      when MUL_MULU => result := mul_result;

      when MUL_MULS => if(a(a'left) /= b(b'left)) then
                         result := mul_neg_result;
                       else
                         result := mul_result;
                       end if;

      when others   => if(a(a'left) = '1') then
                         result := mul_neg_result;
                       else
                         result := mul_result;
                       end if;
    end case;
    return std_ulogic_vector(result);
  end function multiply;


  type state_t is (DISPATCH, RUNNING, ALU_WAIT, BRANCH, BRANCHED, MEM_REQ, MEM_WAIT); 

  type reg_t is record
    state : state_t;
    pc             :  std_ulogic_vector(exec_addr_width_g-1 downto 0);
    inst           : std_ulogic_vector(exec_data_width_g-1 downto 0);
    fwd_dec        : forward_t;
    fwd_dec_result : std_ulogic_vector(exec_data_width_g-1 downto 0);
  end record reg_t;
  constant dflt_reg_c : reg_t :=(
    state          => BRANCHED,
    pc             => (others => '0'),
    inst           => (others => '0'),
    fwd_dec        => dflt_forward_c,
    fwd_dec_result => (others => '0')
  );

  signal r, rin      : reg_t;

  signal s_decode    : decode_out_t;
  signal s_execute   : execute_out_t;

  signal gprfi0_gprf : gprf_out_t;
  signal gprf        : gprf_in_t;

  signal wb_data     : std_ulogic_vector(exec_data_width_g-1 downto 0);

begin
  -----------------------------------------------------------------------------
  -- regfile
  -----------------------------------------------------------------------------
  gprf.ena   <= en_i;
  gprf.adr_a <= s_decode.reg_a;
  gprf.adr_b <= s_decode.reg_b;
  gprf.dat_w <= wb_data;
  gprf.adr_w <= s_decode.ctrl_wrb.reg_d;
  gprf.wre   <= s_decode.ctrl_wrb.reg_write;

  gprfi0 : entity work.gprf_register
    generic map (
      dmem_width_g => exec_data_width_g,
      gprf_size_g  => log2ceil(exec_gprf_size_g)
    )
    port map (
      clk_i     => clk_i,
      reset_n_i => reset_n_i,
      gprf_o    => gprfi0_gprf,
      gprf_i    => gprf
    );

  -----------------------------------------------------------------------------
  -- comb0
  -----------------------------------------------------------------------------
  comb0: process (execute_i, gprfi0_gprf, r) is
    ---------------------------------------------------------------------------
    -- decoder related variables
    ---------------------------------------------------------------------------
    variable v             : reg_t;
    variable instruction   : std_ulogic_vector(exec_data_width_g-1     downto 0);
    variable pc            : std_ulogic_vector(exec_addr_width_g-1     downto 0);
    variable reg_a         : std_ulogic_vector(log2ceil(exec_gprf_size_g)-1 downto 0);
    variable reg_b         : std_ulogic_vector(log2ceil(exec_gprf_size_g)-1 downto 0);
    variable reg_d         : std_ulogic_vector(log2ceil(exec_gprf_size_g)-1 downto 0);
    variable opcode        : std_ulogic_vector(6 downto 0);
    variable inst_rtype    : inst_rtype_t;
    variable inst_itype    : inst_itype_t;
    variable inst_stype    : inst_stype_t;
    variable inst_utype    : inst_utype_t;
    variable inst_iimm     : std_ulogic_vector(exec_data_width_g-1 downto 0);
    variable inst_simm     : std_ulogic_vector(exec_data_width_g-1 downto 0);
    variable inst_bimm     : std_ulogic_vector(exec_data_width_g-1 downto 0);
    variable inst_uimm     : std_ulogic_vector(exec_data_width_g-1 downto 0);
    variable inst_jimm     : std_ulogic_vector(exec_data_width_g-1 downto 0);
    variable irq_masked    : std_ulogic_vector(7 downto 0);

    variable decode        : decode_out_t;

    ---------------------------------------------------------------------------
    -- execution related variables
    ---------------------------------------------------------------------------
    variable alu_src_a     : std_ulogic_vector(exec_data_width_g-1   downto 0);
    variable alu_src_b     : std_ulogic_vector(exec_data_width_g-1   downto 0);
    variable result        : std_ulogic_vector(exec_data_width_g-1   downto 0);
    variable result_add    : std_ulogic_vector(exec_data_width_g-1   downto 0);
    variable result_mul    : std_ulogic_vector(2*exec_data_width_g-1 downto 0);
    variable alu_result    : std_ulogic_vector(exec_data_width_g-1   downto 0);
    variable zero          : std_ulogic;
    variable sign_bits     : std_ulogic_vector(2 downto 0);
    variable carry         : std_ulogic;
    variable carry_out     : std_ulogic;
    variable overflow      : std_ulogic;
    variable negative      : std_ulogic;
    variable dat_a         : std_ulogic_vector(exec_data_width_g-1 downto 0);
    variable dat_b         : std_ulogic_vector(exec_data_width_g-1 downto 0);
    variable sel_dat_a     : std_ulogic_vector(exec_data_width_g-1 downto 0);
    variable sel_dat_b     : std_ulogic_vector(exec_data_width_g-1 downto 0); 
    variable mem_result    : std_ulogic_vector(exec_data_width_g-1 downto 0);
    variable branch        : std_ulogic;
    variable branch_target : std_ulogic_vector(exec_addr_width_g-1 downto 0);

    variable execute       : execute_out_t;

  begin
    v := r;

    ----------------------------------------------------------------------------
    -- source and destination decoding
    -- NOTE: Just for (future?) hazard detection
    ----------------------------------------------------------------------------
    --reg_a  := get_rega(execute_i.inst);
    --reg_b  := get_regb(execute_i.inst);
    --reg_d  := get_regd(execute_i.inst);

    ---------------------------------------------------------------------------
    -- decode
    ---------------------------------------------------------------------------
    v.pc        := execute_i.pc;
    v.inst      := execute_i.inst;

    if(execute_i.stall = '1') then
      pc          := r.pc;
      instruction := r.inst;
    else
      pc          := execute_i.pc;
      instruction := execute_i.inst;
    end if;

    ----------------------------------------------------------------------------
    -- instruction and immediate type generation
    ----------------------------------------------------------------------------
    inst_rtype := get_inst(instruction);
    inst_itype := get_inst(instruction);
    inst_stype := get_inst(instruction);
    inst_utype := get_inst(instruction);

    inst_iimm  := get_iimm(instruction);
    inst_simm  := get_simm(instruction);
    inst_bimm  := get_bimm(instruction);
    inst_uimm  := get_uimm(instruction);
    inst_jimm  := get_jimm(instruction);


    opcode     := get_opcode(instruction);

    -- TODO: The reused decode_out and execute_out types might have elements not required or meant to be register
    --       which we still need as registers. The dafault assignment is to net get trapped by latches
    decode  := dflt_decode_out_c;
    execute := dflt_execute_out_c;

    decode.pc                 := pc;
    decode.ctrl_wrb.reg_d     := inst_rtype.rd;
    decode.reg_a              := inst_rtype.rs1;
    decode.reg_b              := inst_rtype.rs2;
    decode.imm                := (others=>'0');
    decode.ctrl_ex            := dflt_ctrl_execution_c;
    decode.ctrl_mem           := dflt_ctrl_memory_c;
    decode.ctrl_wrb.reg_write := '0';

    -- TODO: place relevant condition (e.g. stall, kill, fetch delay, irq, trap and hazard conditions etc. here)
    if(false) then
    
    elsif(compare(opcode(6 downto 2), inst_load_c)     = '1') then
      --------------------------------------------------------------------------
      --  load is encoded as I-Type instruction
      --------------------------------------------------------------------------
      decode.imm                  := inst_iimm;
      decode.ctrl_ex.alu_op       := ALU_ADD;
      decode.ctrl_ex.alu_src_a    := ALU_SRC_REGA;
      decode.ctrl_ex.alu_src_b    := ALU_SRC_IMM;
      decode.ctrl_mem.mem_write   := '0';
      decode.ctrl_mem.mem_read    := '1';
      decode.ctrl_mem.zero_extend := inst_itype.func3(2); --TODO: modify align_mem_load to support sign extention
      decode.ctrl_wrb.reg_write   := is_not_zero(decode.ctrl_wrb.reg_d);

      case inst_itype.func3(1 downto 0) is
        when "00"   => decode.ctrl_mem.transfer_size := BYTE;
        when "01"   => decode.ctrl_mem.transfer_size := HALFWORD;
        when others => decode.ctrl_mem.transfer_size := WORD;
      end case;

    elsif(compare(opcode(6 downto 2), inst_store_c)    = '1') then
      --------------------------------------------------------------------------
      -- store is S-Type instruction
      --------------------------------------------------------------------------
      decode.imm                  := inst_simm;
      decode.ctrl_ex.alu_op       := ALU_ADD;
      decode.ctrl_ex.alu_src_a    := ALU_SRC_REGA;
      decode.ctrl_ex.alu_src_b    := ALU_SRC_IMM;
      decode.ctrl_mem.mem_write   := '1';
      decode.ctrl_mem.mem_read    := '0';

      case inst_itype.func3(1 downto 0) is
        when "00"   => decode.ctrl_mem.transfer_size := BYTE;
        when "01"   => decode.ctrl_mem.transfer_size := HALFWORD;
        when others => decode.ctrl_mem.transfer_size := WORD;
      end case;

    elsif(compare(opcode(6 downto 2), inst_op_c) = '1' and inst_rtype.func7(0) = '0') then
      --------------------------------------------------------------------------
      -- integer computational instructions (register-register)
      --------------------------------------------------------------------------
      decode.ctrl_wrb.reg_write := is_not_zero(decode.ctrl_wrb.reg_d);

      case inst_rtype.func3 is
        when op_add_c  => decode.ctrl_ex.alu_op      := ALU_ADD;
                          if(inst_rtype.func7(5) = '1') then
                            decode.ctrl_ex.alu_src_b := ALU_SRC_NOT_REGB;
                            decode.ctrl_ex.carry     := CARRY_ONE;
                          end if;

        when op_sll_c  => decode.ctrl_ex.alu_op      := ALU_SHIFT_LEFT;

        when op_slt_c  => decode.ctrl_ex.alu_op      := ALU_COMP;
                          decode.ctrl_ex.alu_src_b   := ALU_SRC_NOT_REGB;
                          decode.ctrl_ex.carry       := CARRY_ONE;

        when op_sltu_c => decode.ctrl_ex.alu_op      := ALU_UCOMP;
                          decode.ctrl_ex.alu_src_b   := ALU_SRC_NOT_REGB;
                          decode.ctrl_ex.carry       := CARRY_ONE;
        
        when op_sr_c   => decode.ctrl_ex.alu_op      := ALU_SHIFT_RIGHT;
                          if(inst_rtype.func7(5) = '1') then
                            decode.ctrl_ex.carry     := CARRY_ARITH;
                          end if;

        when op_or_c   => decode.ctrl_ex.alu_op      := ALU_OR;
        when op_xor_c  => decode.ctrl_ex.alu_op      := ALU_XOR;
        when op_and_c  => decode.ctrl_ex.alu_op      := ALU_AND;

        when others    => null;
      end case;

    elsif(compare(opcode(6 downto 2), inst_op_imm_c)   = '1') then
      --------------------------------------------------------------------------
      -- integer computational instructions (immediate-register)
      --------------------------------------------------------------------------
      decode.ctrl_wrb.reg_write := is_not_zero(decode.ctrl_wrb.reg_d);
      decode.ctrl_ex.alu_src_b  := ALU_SRC_IMM;
      decode.imm                := inst_iimm;

      case inst_itype.func3 is
        when op_add_c  => decode.ctrl_ex.alu_op      := ALU_ADD;
        when op_sll_c  => decode.ctrl_ex.alu_op      := ALU_SHIFT_LEFT;
                          decode.ctrl_ex.csr.illegal := inst_iimm(5);

        when op_slt_c  => decode.ctrl_ex.alu_op      := ALU_COMP;
                          decode.ctrl_ex.alu_src_b   := ALU_SRC_NOT_IMM;
                          decode.ctrl_ex.carry       := CARRY_ONE;

        when op_sltu_c => decode.ctrl_ex.alu_op      := ALU_UCOMP;
                          decode.ctrl_ex.alu_src_b   := ALU_SRC_NOT_IMM;
                          decode.ctrl_ex.carry       := CARRY_ONE;

        when op_sr_c   => decode.ctrl_ex.alu_op      := ALU_SHIFT_RIGHT;
                          decode.ctrl_ex.csr.illegal := inst_iimm(5);
                          if(inst_rtype.func7(5) = '1') then
                            decode.ctrl_ex.carry     := CARRY_ARITH;
                          end if;

        when op_or_c   => decode.ctrl_ex.alu_op      := ALU_OR;
        when op_xor_c  => decode.ctrl_ex.alu_op      := ALU_XOR;
        when op_and_c  => decode.ctrl_ex.alu_op      := ALU_AND;
        when others    => null;
      end case;

    elsif(compare(opcode(6 downto 2), inst_lui_c)      = '1') then
      --------------------------------------------------------------------------
      -- load upper immediate (LSBs set to zero)
      --------------------------------------------------------------------------
      decode.ctrl_wrb.reg_write := is_not_zero(decode.ctrl_wrb.reg_d);
      decode.ctrl_ex.alu_src_a  := ALU_SRC_ZERO;
      decode.ctrl_ex.alu_src_b  := ALU_SRC_IMM;
      decode.imm                := inst_uimm;

    elsif(compare(opcode(6 downto 2), inst_auipc_c)    = '1') then
      --------------------------------------------------------------------------
      -- add upper immediate to PC
      --------------------------------------------------------------------------
      decode.ctrl_wrb.reg_write := is_not_zero(decode.ctrl_wrb.reg_d);
      decode.ctrl_ex.alu_src_a  := ALU_SRC_PC;
      decode.ctrl_ex.alu_src_b  := ALU_SRC_IMM;
      decode.imm                := inst_uimm;

    elsif(compare(opcode(6 downto 2), inst_jal_c)      = '1') then
      --------------------------------------------------------------------------
      -- jump and link (unconditional branch)
      --------------------------------------------------------------------------
      decode.ctrl_wrb.reg_write  := is_not_zero(decode.ctrl_wrb.reg_d);
      decode.ctrl_ex.alu_src_a   := ALU_SRC_PC;
      decode.ctrl_ex.alu_src_b   := ALU_SRC_IMM;
      decode.ctrl_ex.branch_cond := JAL;
      decode.imm                 := inst_jimm;

    elsif(compare(opcode(6 downto 2), inst_jalr_c)     = '1') then
      --------------------------------------------------------------------------
      -- jump and link register (unconditional indirect branch)
      --------------------------------------------------------------------------
      decode.ctrl_wrb.reg_write  := is_not_zero(decode.ctrl_wrb.reg_d);
      decode.ctrl_ex.alu_src_b   := ALU_SRC_IMM;
      decode.ctrl_ex.branch_cond := JALR;
      decode.imm                 := inst_iimm;

    elsif(compare(opcode(6 downto 2), inst_branch_c)   = '1') then
      --------------------------------------------------------------------------
      -- conditional branches 
      --------------------------------------------------------------------------
      decode.imm               := inst_bimm;
      decode.ctrl_ex.alu_src_b := ALU_SRC_NOT_REGB;
      decode.ctrl_ex.carry     := CARRY_ONE;
     
      case inst_stype.func3 is
        when cond_beq_c  => decode.ctrl_ex.branch_cond := BEQ;
        when cond_bne_c  => decode.ctrl_ex.branch_cond := BNE;
        when cond_blt_c  => decode.ctrl_ex.branch_cond := BLT;
        when cond_bge_c  => decode.ctrl_ex.branch_cond := BGE;
        when cond_bltu_c => decode.ctrl_ex.branch_cond := BLTU;
        when cond_bgeu_c => decode.ctrl_ex.branch_cond := BGEU;
        when others      => decode.ctrl_ex.branch_cond := JAL;
      end case;

    elsif(compare(opcode(6 downto 2), inst_misc_mem_c) = '1') then
      --------------------------------------------------------------------------
      -- memory fence instructions (currently not supported)
      --------------------------------------------------------------------------
      --assert False report "misc mem instructions are not implemented yet" severity failure;
      decode.ctrl_ex.csr.illegal := '1';

    elsif(compare(opcode(6 downto 2), inst_system_c)   = '1') then
      --------------------------------------------------------------------------
      -- system instruction (could be emulated as traps)
      --------------------------------------------------------------------------
      --assert False report "system calls are not implemented yet" severity failure;
      decode.ctrl_ex.csr.illegal := '1';

    elsif(use_m_ext_g and compare(opcode(6 downto 2), inst_op_c) = '1' and inst_rtype.func7(0) = '1') then
      --------------------------------------------------------------------------
      -- 32 bit M extention
      --------------------------------------------------------------------------
      decode.ctrl_wrb.reg_write := is_not_zero(decode.ctrl_wrb.reg_d);

      -- serial divider requires stalling the pipeline
      if(inst_rtype.func3(2) = '1') then
        --assert False report "pipeline stall not implemented yes" severity failure;
        --if(r.decode.stall = '0') then
        --  decode.stall          := '1';
        --end if;
      end if;

      case inst_rtype.func3 is
        when op_mul_c    => decode.ctrl_ex.alu_op := ALU_MUL;
                            decode.ctrl_ex.mul_op := MUL_MULU;

        when op_mulh_c   => decode.ctrl_ex.alu_op := ALU_MULH;
                            decode.ctrl_ex.mul_op := MUL_MULS;

        when op_mulhsu_c => decode.ctrl_ex.alu_op := ALU_MULH;
                            decode.ctrl_ex.mul_op := MUL_MULSU;

        when op_mulhu_c  => decode.ctrl_ex.alu_op := ALU_MULH;
                            decode.ctrl_ex.mul_op := MUL_MULU;

        when op_div_c    => decode.ctrl_ex.alu_op := ALU_DIV;
                            decode.ctrl_ex.div_op := DIV_DIVS;

        when op_divu_c   => decode.ctrl_ex.alu_op := ALU_DIV;
                            decode.ctrl_ex.div_op := DIV_DIVU;

        when op_rem_c    => decode.ctrl_ex.alu_op := ALU_REM;
                            decode.ctrl_ex.div_op := DIV_DIVS;

        when op_remu_c   => decode.ctrl_ex.alu_op := ALU_REM;
                            decode.ctrl_ex.div_op := DIV_DIVU;

        when others      => null;
      end case;
    else
      decode.ctrl_ex.csr.illegal := '1';
    end if;

    -- TODO: just for debugging
    s_decode <= decode;

    
    ---------------------------------------------------------------------------
    -- execute
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- TODO: select source data from regfile
    -- short bypass from regfile vs. forward pass within regfile 
    ---------------------------------------------------------------------------
    sel_dat_a := select_register_data(gprfi0_gprf.dat_a, decode.reg_a, r.fwd_dec_result, fwd_cond(r.fwd_dec.reg_write, r.fwd_dec.reg_d, decode.reg_a));
    sel_dat_b := select_register_data(gprfi0_gprf.dat_b, decode.reg_b, r.fwd_dec_result, fwd_cond(r.fwd_dec.reg_write, r.fwd_dec.reg_d, decode.reg_b));

    ---------------------------------------------------------------------------
    -- conditional flush execution in case of control hazards (branches)
    -- not clear if we need this becaus of our two stage pipeline
    -- TODO:
    --   |-> clear any write backs from decoder
    --   |-> clear any load store unit requests from decoder
    ---------------------------------------------------------------------------
    if(false) then

    else

    end if;

    ---------------------------------------------------------------------------
    -- TODO: data from load store unit
    --  |-> define functionality of load store unit
    ---------------------------------------------------------------------------
    mem_result := (others => '0');

    ---------------------------------------------------------------------------
    -- multiplex between the variaous source data paths
    ---------------------------------------------------------------------------
    dat_a := sel_dat_a;
    dat_b := sel_dat_b;

    ---------------------------------------------------------------------------
    -- TODO: place data interface to load store unit here
    ---------------------------------------------------------------------------
    execute.dat_b := dat_b;


    ----------------------------------------------------------------------------
    -- set the first operand of the ALU
    ----------------------------------------------------------------------------
    case(decode.ctrl_ex.alu_src_a) is
      when ALU_SRC_PC       => alu_src_a := sign_extend(decode.pc, '0', exec_data_width_g);
      when ALU_SRC_ZERO     => alu_src_a := (others => '0');
      when ALU_SRC_ZIMM     => alu_src_a := sign_extend(reg_a, '0', exec_data_width_g);
      when others           => alu_src_a := dat_a;
    end case;

    ----------------------------------------------------------------------------
    -- set the second operand of the ALU
    ----------------------------------------------------------------------------
    case(decode.ctrl_ex.alu_src_b) is
      when ALU_SRC_IMM      => alu_src_b := decode.imm;
      when ALU_SRC_NOT_IMM  => alu_src_b := not decode.imm;
      when ALU_SRC_NOT_REGB => alu_src_b := not dat_b;
      when others           => alu_src_b := dat_b;
    end case;

    ----------------------------------------------------------------------------
    -- determine value of carry in
    ----------------------------------------------------------------------------
    case(decode.ctrl_ex.carry) is
      when CARRY_ONE   => carry := '1';
      when CARRY_ARITH => carry := alu_src_a(exec_data_width_g-1);
      when others      => carry := '0';
    end case;

    result_add := add(alu_src_a, alu_src_b, carry);

    ----------------------------------------------------------------------------
    -- generate flags used to evaluate branch condition
    ----------------------------------------------------------------------------
    zero      := is_zero(result_add(result_add'left-1 downto 0));
    negative  := result_add(exec_data_width_g-1);
    sign_bits := result_add(exec_data_width_g-1) & alu_src_a(exec_data_width_g-1) & alu_src_b(exec_data_width_g-1);

    case(sign_bits) is
      when "001" | "010" | "011" | "111" 
                  => carry_out := '1';
      when others => carry_out := '0';
    end case;

    overflow  := (    result_add(exec_data_width_g-1) and not alu_src_a(exec_data_width_g-1) and not alu_src_b(exec_data_width_g-1)) or
                 (not result_add(exec_data_width_g-1) and     alu_src_a(exec_data_width_g-1) and     alu_src_b(exec_data_width_g-1));

    ----------------------------------------------------------------------------
    -- multiplication
    ----------------------------------------------------------------------------
    if(use_m_ext_g) then
      result_mul := multiply(dat_a, dat_b, decode.ctrl_ex.mul_op);
    else
      result_mul := (others=>'0');
    end if;

    ----------------------------------------------------------------------------
    -- arithmetic ALU operations
    ----------------------------------------------------------------------------
    case(decode.ctrl_ex.alu_op) is
      when ALU_ADD         => result := result_add;
      when ALU_OR          => result := (alu_src_a or alu_src_b);
      when ALU_AND         => result := (alu_src_a and alu_src_b);
      when ALU_XOR         => result := (alu_src_a xor alu_src_b);
      when ALU_SHIFT_LEFT  => result := shift_left(alu_src_a, alu_src_b(4 downto 0));
      when ALU_SHIFT_RIGHT => result := shift_right(alu_src_a, alu_src_b(4 downto 0), carry);
      when ALU_UCOMP       => result := (result'left downto 1 => '0') & not carry_out;
      when ALU_COMP        => result := (result'left downto 1 => '0') & (negative xor overflow);
      when ALU_MUL         => result := result_mul(exec_data_width_g-1 downto 0);
      when ALU_MULH        => result := result_mul(2*exec_data_width_g-1 downto exec_data_width_g);
      when ALU_DIV         => result := (others => '0'); --div_blocki0_quotient;
      when ALU_REM         => result := (others => '0'); --div_blocki0_reminder;
      when others          => null;
    end case;

    ----------------------------------------------------------------------------
    -- mux with csr data
    ----------------------------------------------------------------------------
    --if(exec_i.ctrl_ex.csr.csr_en = '1') then
    --  result := csrfi0_csrf.rdata;
    --end if;

    ----------------------------------------------------------------------------
    -- mux result with PC increment 
    ----------------------------------------------------------------------------
    case(decode.ctrl_ex.branch_cond) is
      when JAL | JALR => alu_result := sign_extend(std_ulogic_vector(unsigned(decode.pc(decode.pc'left downto 2)) + 1) & "00", '0', exec_data_width_g);
      when others     => alu_result := result;
    end case;

    ----------------------------------------------------------------------------
    -- branch target address generation
    ----------------------------------------------------------------------------
    case(decode.ctrl_ex.branch_cond) is
      when JAL | JALR => branch_target := result(result'left downto 1) & '0';
      when others     => branch_target := std_ulogic_vector(signed(decode.pc(decode.pc'left downto 1)) + signed(decode.imm(decode.pc'left downto 1))) & '0';
    end case;


    ---------------------------------------------------------------------------
    -- TODO: exception handling
    ---------------------------------------------------------------------------

    --if(r.exec.flush_ex = '0') then
    --  if(csrfi0_csrf.exception = '1') then
    --    v.exec.ctrl_wrb.reg_write := '0';
    --    v.exec.ctrl_wrb.reg_d     := (others=>'0');
    --  end if;
    --end if;

    --if(csrfi0_csrf.exception = '1') then
    --  branch_target      := csrfi0_csrf.csr.evec;
    --elsif(exec_i.ctrl_ex.csr.sret = '1') then
    --  branch_target      := csrfi0_csrf.csr.epc;
    --end if;


    ----------------------------------------------------------------------------
    -- evaluate branch condition
    ----------------------------------------------------------------------------
    if(false) then -- TODO: place flush, kill, ... condition here
      branch := '0';
    elsif(false) then -- TODO: place exception condition here
      branch := '1';
    elsif(false) then -- TODO: place return from exception here
      branch := '1';
    else
      -- determine branch condition
      case(decode.ctrl_ex.branch_cond) is
        when JAL | JALR => branch := '1';
        when BEQ        => branch := zero;
        when BNE        => branch := not zero;
        when BLT        => branch := negative xor overflow;
        when BGE        => branch := not (negative xor overflow);
        when BLTU       => branch := not carry_out;
        when BGEU       => branch := carry_out;
        when others     => branch := '0';
      end case;
    end if;

    -- TODO: combinatorial vs. registered?
    execute_o.branch        <= branch;
    execute_o.branch_target <= branch_target;

    ---------------------------------------------------------------------------
    -- controller
    ---------------------------------------------------------------------------
    fsm0: case(r.state) is
      when BRANCHED => v.state := DISPATCH;

      when DISPATCH => v.state := RUNNING;

      when RUNNING  => v.state := RUNNING;

      when ALU_WAIT => v.state := RUNNING;

      when MEM_REQ  => v.state := RUNNING;

      when MEM_WAIT => v.state := RUNNING;
      
      when others   => null;
    end case fsm0;

    v.fwd_dec        := decode.ctrl_wrb;
    v.fwd_dec_result := alu_result;

    -- TODO: just for debugging
    s_execute <= execute;

    -- TODO: multiplex with memory load
    wb_data   <= alu_result;

    rin <= v;
  end process comb0;

  -----------------------------------------------------------------------------
  -- sync0
  -----------------------------------------------------------------------------
  sync0: process (clk_i, reset_n_i) is
  begin
    if(reset_n_i = '0') then
      r <= dflt_reg_c;
    elsif(rising_edge(clk_i)) then
      if(en_i = '1') then
        if(init_i = '1') then
          r <= dflt_reg_c;
        else
          r <= rin;
        end if;
      end if;
    end if;
  end process sync0;
  
end architecture rtl;

