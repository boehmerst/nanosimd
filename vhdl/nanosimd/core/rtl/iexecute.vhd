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
    use_reg_fwd_wrb_g : boolean := true;
    use_mem_fwd_wrb_g : boolean := true; 
    use_m_ext_g       : boolean := false
  );
  port (
    clk_i     : in  std_ulogic;
    reset_n_i : in  std_ulogic;
    init_i    : in  std_ulogic;
    en_i      : in  std_ulogic;
    fetch_i   : in  fetch_out_t;
    fetch_o   : out fetch_in_t;
    dmem_i    : in  dmem_in_t;
    dmem_o    : out dmem_out_t
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

  signal gprfi0_gprf       : gprf_out_t;
  signal gprf              : gprf_in_t;

  signal wb_data           : std_ulogic_vector(exec_data_width_g-1 downto 0);

  signal stall_r           : std_ulogic; -- TODO: dirty hack

  signal decode            : decode_in_t;
  signal decodei0_decode   : decode_out_t;

  signal execute           : execute_in_t;
  signal executei0_execute : execute_out_t;

  signal mem               : mem_in_t;
  signal memi0_mem         : mem_out_t;

begin
  -----------------------------------------------------------------------------
  -- regfile
  -----------------------------------------------------------------------------
  gprf.ena   <= en_i;
  gprf.adr_a <= decodei0_decode.reg_a;
  gprf.adr_b <= decodei0_decode.reg_b;
  gprf.dat_w <= wb_data;
  gprf.adr_w <= decodei0_decode.ctrl_wrb.reg_d;
  gprf.wre   <= decodei0_decode.ctrl_wrb.reg_write;

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
  -- decoder
  -----------------------------------------------------------------------------
  decode.irq.pending  <= (others => '0');               --execi0_status.ip;
  decode.irq.mask     <= (others => '0');               --execi0_status.im;
  decode.irq.enable   <= '0';                           --execi0_status.ei;
  decode.pc           <= fetch_i.pc;
  decode.inst         <= fetch_i.inst;
  decode.ctrl_mem_wrb <= memi0_mem.ctrl_mem_wrb;
  decode.mem_result   <= dmem_i.data;                   -- TODO: implement LS unit
  decode.alu_result   <= executei0_execute.alu_result;  --memi0_mem.alu_result;
  decode.flush_id     <= executei0_execute.flush_id;  
  decode.mode         <= '0';                           --execi0_status.s;

  decodei0: block is
    port (
      clk_i     : in  std_ulogic;
      reset_n_i : in  std_ulogic;
      init_i    : in  std_ulogic;
      en_i      : in  std_ulogic;
      decode_i  : in  decode_in_t;
      decode_o  : out decode_out_t
    );
    port map (
      clk_i     => clk_i,
      reset_n_i => reset_n_i,
      init_i    => init_i,
      en_i      => en_i,
      decode_i  => decode,
      decode_o  => decodei0_decode
    );

    type reg_t is record
      pc    : std_ulogic_vector(exec_addr_width_g-1 downto 0);
      inst  : std_ulogic_vector(exec_data_width_g-1 downto 0);
      stall : std_ulogic;
    end record reg_t;
    constant dflt_reg_c : reg_t :=(
      pc    => (others => '0'),
      inst  => (others => '0'),
      stall => '0'
    );

    signal r, rin : reg_t;

  begin
    -----------------------------------------------------------------------------
    -- comb0
    -----------------------------------------------------------------------------
    comb0: process (r, decode_i) is

      variable v             : reg_t;
      variable wb_result     : std_ulogic_vector(exec_data_width_g-1          downto 0);
      variable instruction   : std_ulogic_vector(exec_data_width_g-1          downto 0);
      variable pc            : std_ulogic_vector(exec_addr_width_g-1          downto 0);
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

    begin
      v := r;

      v.pc          := decode_i.pc;
      v.inst        := decode_i.inst;

      ----------------------------------------------------------------------------
      -- write-back multiplexor
      ----------------------------------------------------------------------------
      if(decode_i.ctrl_mem_wrb.mem_read = '1') then
        wb_result   := align_mem_load(decode_i.mem_result, decode_i.ctrl_mem_wrb.transfer_size, decode_i.alu_result(1 downto 0), decode_i.ctrl_mem_wrb.zero_extend);
      else
        wb_result   := decode_i.alu_result;
      end if;

      ----------------------------------------------------------------------------
      -- TODO: hazard, stall, etc.  processing
      -- to follow memory load instructions
      ----------------------------------------------------------------------------
      if(r.stall = '1') then
        pc          := r.pc;
        instruction := r.inst;
        v.stall     := '0';
      else
        pc          := decode_i.pc;
        instruction := decode_i.inst;
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

      decode.pc                 := pc;
      decode.ctrl_wrb.reg_d     := inst_rtype.rd;
      decode.reg_a              := inst_rtype.rs1;
      decode.reg_b              := inst_rtype.rs2;
      decode.imm                := (others=>'0');
      decode.ctrl_ex            := dflt_ctrl_execution_c;
      decode.ctrl_mem           := dflt_ctrl_memory_c;
      decode.ctrl_wrb.reg_write := '0';

      -- TODO: place relevant condition (e.g. stall, kill, fetch delay, irq, trap and hazard conditions etc. here)
      if(decode_i.flush_id = '1') then
        -- NOTE: just for readability
        decode.pc             := (others=>'0');
        decode.ctrl_wrb.reg_d := (others=>'0');
        decode.reg_a          := (others=>'0');
        decode.reg_b          := (others=>'0');
        decode.imm            := (others=>'0');
      elsif(compare(opcode(6 downto 2), inst_load_c)     = '1') then
        --------------------------------------------------------------------------
        --  load is encoded as I-Type instruction
        --------------------------------------------------------------------------
        if r.stall = '0' then
          v.stall                   := '1'; -- TODO: better use dedicated stall conditions
        end if;

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

      ----------------------------------------------------------------------------
      -- latch current instruction that causes the stall
      ----------------------------------------------------------------------------
      if(v.stall = '1') then
        v.pc   := pc;
        v.inst := instruction;
      end if;

      decode.stall := v.stall;

      stall_r <= r.stall;

      rin      <= v;
      wb_data  <= wb_result;
      decode_o <= decode;

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

  end block decodei0;


  -----------------------------------------------------------------------------
  -- execution
  -----------------------------------------------------------------------------
  execute.dat_a          <= gprfi0_gprf.dat_a;
  execute.dat_b          <= gprfi0_gprf.dat_b;
  execute.reg_a          <= decodei0_decode.reg_a;
  execute.reg_b          <= decodei0_decode.reg_b;

  execute.imm            <= decodei0_decode.imm;
  execute.pc             <= decodei0_decode.pc;
  execute.stall          <= decodei0_decode.stall;
  execute.ctrl_wrb       <= decodei0_decode.ctrl_wrb;
  execute.ctrl_mem       <= decodei0_decode.ctrl_mem;
  execute.ctrl_ex        <= decodei0_decode.ctrl_ex;

  executei0: block is
    port (
      clk_i     : in  std_ulogic;
      reset_n_i : in  std_ulogic;
      init_i    : in  std_ulogic;
      en_i      : in  std_ulogic;
      execute_i : in  execute_in_t;
      execute_o : out execute_out_t
    );
    port map (
      clk_i     => clk_i,
      reset_n_i => reset_n_i,
      init_i    => init_i,
      en_i      => en_i,
      execute_i => execute,
      execute_o => executei0_execute
    );

    type state_t is (ST_DISPATCH, ST_RUNNING, ST_ALU_WAIT, ST_BRANCH, ST_BRANCHED, ST_MEM_REQ, ST_MEM_WAIT);

    type reg_t is record
      state      : state_t;
      flush      : std_ulogic;
      ctrl_wrb   : forward_t;
      ctrl_mem   : ctrl_memory_t;
      alu_result : std_ulogic_vector(exec_data_width_g-1 downto 0);
    end record reg_t;
    constant dflt_reg_c : reg_t :=(
      state      => ST_BRANCHED,
      flush      => '0',
      ctrl_wrb   => dflt_forward_c,
      ctrl_mem   => dflt_ctrl_memory_c,
      alu_result => (others => '0')
    );

    signal r, rin : reg_t;

  begin
    ---------------------------------------------------------------------------
    -- comb0
    ---------------------------------------------------------------------------
    comb0: process (r, execute_i) is
      variable v : reg_t;
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

      execute := dflt_execute_out_c;

      sel_dat_a := select_register_data(execute_i.dat_a, execute_i.reg_a, r.alu_result, '0');
      sel_dat_b := select_register_data(execute_i.dat_b, execute_i.reg_b, r.alu_result, '0');

      ---------------------------------------------------------------------------
      -- conditional flush execution in case of control hazards (branches)
      ---------------------------------------------------------------------------
      if(r.flush = '1') then
        v.ctrl_mem.mem_write := '0';
        v.ctrl_mem.mem_read  := '0';
        v.ctrl_wrb.reg_write := '0';
        v.ctrl_wrb.reg_d     := (others=>'0');
      else
        v.ctrl_mem           := execute_i.ctrl_mem;
        v.ctrl_wrb           := execute_i.ctrl_wrb;
      end if;

      dat_a      := sel_dat_a;
      dat_b      := sel_dat_b;

      ---------------------------------------------------------------------------
      -- TODO: place data interface to load store unit here
      ---------------------------------------------------------------------------
      execute.dat_b := dat_b;

      ----------------------------------------------------------------------------
      -- set the first operand of the ALU
      ----------------------------------------------------------------------------
      case(execute_i.ctrl_ex.alu_src_a) is
        when ALU_SRC_PC       => alu_src_a := sign_extend(execute_i.pc, '0', exec_data_width_g);
        when ALU_SRC_ZERO     => alu_src_a := (others => '0');
        when ALU_SRC_ZIMM     => alu_src_a := sign_extend(execute_i.reg_a, '0', exec_data_width_g);
        when others           => alu_src_a := dat_a;
      end case;

      ----------------------------------------------------------------------------
      -- set the second operand of the ALU
      ----------------------------------------------------------------------------
      case(execute_i.ctrl_ex.alu_src_b) is
        when ALU_SRC_IMM      => alu_src_b := execute_i.imm;
        when ALU_SRC_NOT_IMM  => alu_src_b := not execute_i.imm;
        when ALU_SRC_NOT_REGB => alu_src_b := not dat_b;
        when others           => alu_src_b := dat_b;
      end case;

      ----------------------------------------------------------------------------
      -- determine value of carry in
      ----------------------------------------------------------------------------
      case(execute_i.ctrl_ex.carry) is
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
        result_mul := multiply(dat_a, dat_b, execute_i.ctrl_ex.mul_op);
      else
        result_mul := (others=>'0');
      end if;

      ----------------------------------------------------------------------------
      -- arithmetic ALU operations
      ----------------------------------------------------------------------------
      case(execute_i.ctrl_ex.alu_op) is
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
      case(execute_i.ctrl_ex.branch_cond) is
        when JAL | JALR => alu_result := sign_extend(std_ulogic_vector(unsigned(execute_i.pc(execute_i.pc'left downto 2)) + 1) & "00", '0', exec_data_width_g);
        when others     => alu_result := result;
      end case;

      ----------------------------------------------------------------------------
      -- branch target address generation
      ----------------------------------------------------------------------------
      case(execute_i.ctrl_ex.branch_cond) is
        when JAL | JALR => branch_target := result(result'left downto 1) & '0';
        when others     => branch_target := std_ulogic_vector(signed(execute_i.pc(execute_i.pc'left downto 1)) + signed(execute_i.imm(execute_i.pc'left downto 1))) & '0';
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
      if(r.flush) then -- TODO: place flush, kill, ... condition here
        branch := '0';
      elsif(false) then -- TODO: place exception condition here
        branch := '1';
      elsif(false) then -- TODO: place return from exception here
        branch := '1';
      else
        -- determine branch condition
        case(execute_i.ctrl_ex.branch_cond) is
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

      execute.branch        := branch;
      execute.branch_target := branch_target;

      ---------------------------------------------------------------------------
      -- controller
      ---------------------------------------------------------------------------
      v.flush := '0'; -- TODO: for now we assume one cycle delay to recover from branch -> clear
    
      fsm0: case(r.state) is
        when ST_BRANCH   => v.state := ST_BRANCHED;

        when ST_BRANCHED => v.state := ST_DISPATCH;

        when ST_DISPATCH => v.state := ST_RUNNING;

        when ST_RUNNING  => v.state := ST_RUNNING;
                            if branch = '1' then
                              v.state := ST_BRANCH;
                              v.flush := '1';
                            end if;

        when ST_ALU_WAIT => v.state := ST_RUNNING;

        when ST_MEM_REQ  => v.state := ST_RUNNING;

        when ST_MEM_WAIT => v.state := ST_RUNNING;
      
        when others   => null;
      end case fsm0;
      
      execute.alu_result := alu_result;
      execute.flush_id   := r.flush;
      execute.ctrl_mem   := v.ctrl_mem;

      execute_o <= execute;
      rin       <= v;
    end process comb0;

    -----------------------------------------------------------------------------
    -- sync0
    -----------------------------------------------------------------------------
    sync0: process (clk_i, reset_n_i) is
    begin
      if(reset_n_i = '0') then
        r <= dflt_reg_c;
      elsif(rising_edge(clk_i)) then
        if(en_i = '1' and execute_i.stall = '0') then
          if(init_i = '1') then
            r <= dflt_reg_c;
          else
            r <= rin;
          end if;
        end if;
      end if;
    end process sync0;

  end block executei0;

  fetch_o.stall         <= decodei0_decode.stall;
  fetch_o.branch        <= executei0_execute.branch;
  fetch_o.branch_target <= executei0_execute.branch_target;

  -----------------------------------------------------------------------------
  -- memory access (don't call it a load-store unit)
  -----------------------------------------------------------------------------
  mem.mem_addr   <= executei0_execute.alu_result;
  mem.dat_b      <= executei0_execute.dat_b;
  mem.ctrl_mem   <= executei0_execute.ctrl_mem;

  memi0: block is
    port (
      clk_i     : in  std_ulogic;
      reset_n_i : in  std_ulogic;
      init_i    : in  std_ulogic;
      en_i      : in  std_ulogic;
      mem_i     : in  mem_in_t;
      mem_o     : out mem_out_t;
      dmem_i    : in  dmem_in_t;
      dmem_o    : out dmem_out_t
    );
    port map (
      clk_i     => clk_i,
      reset_n_i => reset_n_i,
      init_i    => init_i,
      en_i      => en_i,
      mem_i     => mem,
      mem_o     => memi0_mem,
      dmem_i    => dmem_i,
      dmem_o    => dmem_o
    );

    type reg_t is record
      mem          : mem_out_t;
      mem_addr_sel : std_ulogic_vector(log2ceil(exec_data_width_g/8)-1 downto 0);
    end record reg_t;
    constant dflt_reg_c : reg_t :=(
      mem          => dflt_mem_out_c,
      mem_addr_sel => (others=>'0')
    );

    signal r, rin : reg_t;

  begin
    ------------------------------------------------------------------------------
    -- comb0
    ------------------------------------------------------------------------------
    comb0: process(r, mem_i, stall_r) is
      variable v            : reg_t;
    begin
      v := r;

      ----------------------------------------------------------------------------
      -- simple delay latch to provide to WB stage and forward to EX
      ----------------------------------------------------------------------------
      v.mem.ctrl_mem_wrb.mem_read      := mem_i.ctrl_mem.mem_read and not stall_r;
      v.mem.ctrl_mem_wrb.transfer_size := mem_i.ctrl_mem.transfer_size;

      ---------------------------------------------------------------------------
      -- drive module outputs
      ---------------------------------------------------------------------------
      mem_o       <= r.mem;
      dmem_o.data <= align_mem_store(mem_i.dat_b, mem_i.ctrl_mem.transfer_size);
      dmem_o.sel  <= decode_mem_store(mem_i.mem_addr(1 downto 0), mem_i.ctrl_mem.transfer_size);
      dmem_o.we   <= mem_i.ctrl_mem.mem_write;
      dmem_o.addr <= mem_i.mem_addr(exec_addr_width_g-1 downto 0);
      dmem_o.en   <= mem_i.ctrl_mem.mem_read or mem_i.ctrl_mem.mem_write;

      rin <= v;
    end process comb0;

    ------------------------------------------------------------------------------
    -- sync0
    ------------------------------------------------------------------------------
    sync0: process(clk_i, reset_n_i) is
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

  end block memi0;

end architecture rtl;

