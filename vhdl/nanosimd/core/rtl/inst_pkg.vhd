library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.func_pkg.all;

package inst_pkg is

  constant inst_length32_c : std_ulogic_vector(1 downto 0) := "11";

  constant inst_load_c     : std_ulogic_vector(4 downto 0) := "00000";
  constant inst_store_c    : std_ulogic_vector(4 downto 0) := "01000";
  constant inst_branch_c   : std_ulogic_vector(4 downto 0) := "11000"; 
  constant inst_jalr_c     : std_ulogic_vector(4 downto 0) := "11001"; 
  constant inst_misc_mem_c : std_ulogic_vector(4 downto 0) := "00011"; 
  constant inst_jal_c      : std_ulogic_vector(4 downto 0) := "11011"; 
  constant inst_op_imm_c   : std_ulogic_vector(4 downto 0) := "00100"; 
  constant inst_op_c       : std_ulogic_vector(4 downto 0) := "01100";
  constant inst_system_c   : std_ulogic_vector(4 downto 0) := "11100";
  constant inst_auipc_c    : std_ulogic_vector(4 downto 0) := "00101";  
  constant inst_lui_c      : std_ulogic_vector(4 downto 0) := "01101";

  constant op_add_c        : std_ulogic_vector(2 downto 0) := "000";
  constant op_sll_c        : std_ulogic_vector(2 downto 0) := "001";
  constant op_slt_c        : std_ulogic_vector(2 downto 0) := "010";
  constant op_sltu_c       : std_ulogic_vector(2 downto 0) := "011";
  constant op_xor_c        : std_ulogic_vector(2 downto 0) := "100";
  constant op_sr_c         : std_ulogic_vector(2 downto 0) := "101";
  constant op_or_c         : std_ulogic_vector(2 downto 0) := "110";
  constant op_and_c        : std_ulogic_vector(2 downto 0) := "111";

  constant op_mul_c        : std_ulogic_vector(2 downto 0) := "000";
  constant op_mulh_c       : std_ulogic_vector(2 downto 0) := "001";
  constant op_mulhsu_c     : std_ulogic_vector(2 downto 0) := "010";
  constant op_mulhu_c      : std_ulogic_vector(2 downto 0) := "011";
  constant op_div_c        : std_ulogic_vector(2 downto 0) := "100";
  constant op_divu_c       : std_ulogic_vector(2 downto 0) := "101";
  constant op_rem_c        : std_ulogic_vector(2 downto 0) := "110";
  constant op_remu_c       : std_ulogic_vector(2 downto 0) := "111";  

  constant cond_beq_c      : std_ulogic_vector(2 downto 0) := "000";
  constant cond_bne_c      : std_ulogic_vector(2 downto 0) := "001";
  constant cond_blt_c      : std_ulogic_vector(2 downto 0) := "100";
  constant cond_bge_c      : std_ulogic_vector(2 downto 0) := "101";
  constant cond_bltu_c     : std_ulogic_vector(2 downto 0) := "110";
  constant cond_bgeu_c     : std_ulogic_vector(2 downto 0) := "111";

  type inst_rtype_t is record
    opcode : std_ulogic_vector(6 downto 0);
    rd     : std_ulogic_vector(4 downto 0);
    func3  : std_ulogic_vector(2 downto 0);
    rs1    : std_ulogic_vector(4 downto 0);
    rs2    : std_ulogic_vector(4 downto 0);
    func7  : std_ulogic_vector(6 downto 0);
  end record inst_rtype_t;

  type inst_itype_t is record
    opcode : std_ulogic_vector( 6 downto 0);
    rd     : std_ulogic_vector( 4 downto 0);
    func3  : std_ulogic_vector( 2 downto 0);
    rs1    : std_ulogic_vector( 4 downto 0);
    imm12  : std_ulogic_vector(11 downto 0);
  end record inst_itype_t;

  type inst_stype_t is record
    opcode : std_ulogic_vector(6 downto 0);
    imm5   : std_ulogic_vector(4 downto 0);
    func3  : std_ulogic_vector(2 downto 0);
    rs1    : std_ulogic_vector(4 downto 0);
    rs2    : std_ulogic_vector(4 downto 0);
    imm7   : std_ulogic_vector(6 downto 0);
  end record inst_stype_t;

  type inst_utype_t is record
    opcode : std_ulogic_vector( 6 downto 0);
    rd     : std_ulogic_vector( 4 downto 0);
    imm20  : std_ulogic_vector(19 downto 0);
  end record inst_utype_t;

  type csr_addr_t is record
    fflags    : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    frm       : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    fcsr      : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    sup0      : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    sup1      : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    epc       : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    badvaddr  : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    ptbr      : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    asid      : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    count     : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    compare   : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    evec      : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    cause     : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    status    : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    hartid    : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    impl      : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    fatc      : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    send_ipi  : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    clear_ipi : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    stats     : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    reset     : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    tohost    : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    fromhost  : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    cycle     : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    cycleh    : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    time      : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    timeh     : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    instret   : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    instreth  : std_ulogic_vector(csr_addr_width_c-1 downto 0);
  end record csr_addr_t;
  constant csr_addr_c : csr_addr_t :=(
    fflags    => std_ulogic_vector(to_unsigned(16#001#, csr_addr_width_c)),
    frm       => std_ulogic_vector(to_unsigned(16#002#, csr_addr_width_c)),
    fcsr      => std_ulogic_vector(to_unsigned(16#003#, csr_addr_width_c)),
    sup0      => std_ulogic_vector(to_unsigned(16#500#, csr_addr_width_c)),
    sup1      => std_ulogic_vector(to_unsigned(16#501#, csr_addr_width_c)),
    epc       => std_ulogic_vector(to_unsigned(16#502#, csr_addr_width_c)),
    badvaddr  => std_ulogic_vector(to_unsigned(16#503#, csr_addr_width_c)),
    ptbr      => std_ulogic_vector(to_unsigned(16#504#, csr_addr_width_c)),
    asid      => std_ulogic_vector(to_unsigned(16#505#, csr_addr_width_c)),
    count     => std_ulogic_vector(to_unsigned(16#506#, csr_addr_width_c)),
    compare   => std_ulogic_vector(to_unsigned(16#507#, csr_addr_width_c)),
    evec      => std_ulogic_vector(to_unsigned(16#508#, csr_addr_width_c)),
    cause     => std_ulogic_vector(to_unsigned(16#509#, csr_addr_width_c)),
    status    => std_ulogic_vector(to_unsigned(16#50a#, csr_addr_width_c)),
    hartid    => std_ulogic_vector(to_unsigned(16#50b#, csr_addr_width_c)),
    impl      => std_ulogic_vector(to_unsigned(16#50c#, csr_addr_width_c)),
    fatc      => std_ulogic_vector(to_unsigned(16#50d#, csr_addr_width_c)),
    send_ipi  => std_ulogic_vector(to_unsigned(16#50e#, csr_addr_width_c)),
    clear_ipi => std_ulogic_vector(to_unsigned(16#50f#, csr_addr_width_c)),
    stats     => std_ulogic_vector(to_unsigned(16#51c#, csr_addr_width_c)),
    reset     => std_ulogic_vector(to_unsigned(16#51d#, csr_addr_width_c)),
    tohost    => std_ulogic_vector(to_unsigned(16#51e#, csr_addr_width_c)),
    fromhost  => std_ulogic_vector(to_unsigned(16#51f#, csr_addr_width_c)),
    cycle     => std_ulogic_vector(to_unsigned(16#c00#, csr_addr_width_c)),
    cycleh    => std_ulogic_vector(to_unsigned(16#c80#, csr_addr_width_c)),
    time      => std_ulogic_vector(to_unsigned(16#c01#, csr_addr_width_c)),
    timeh     => std_ulogic_vector(to_unsigned(16#c81#, csr_addr_width_c)),
    instret   => std_ulogic_vector(to_unsigned(16#c02#, csr_addr_width_c)),
    instreth  => std_ulogic_vector(to_unsigned(16#c82#, csr_addr_width_c))
  );

  constant irq_timer_c : integer := 7;

  type csr_status_t is record
    ip  : std_ulogic_vector(7 downto 0);
    im  : std_ulogic_vector(7 downto 0);
    er  : std_ulogic;
    vm  : std_ulogic;
    s64 : std_ulogic;
    u64 : std_ulogic;
    ef  : std_ulogic;
    pei : std_ulogic;
    ei  : std_ulogic;
    ps  : std_ulogic;
    s   : std_ulogic;
  end record csr_status_t;
  constant dflt_csr_status_c : csr_status_t :=(
    ip  => (others=>'0'),
    im  => (others=>'0'),
    er  => '0',
    vm  => '0',
    s64 => '0',
    u64 => '0',
    ef  => '0',
    pei => '0',
    ei  => '0',
    ps  => '0',
    s   => '1'
  );

  type csr_reg_t is record
    status      : csr_status_t;
    fflags      : std_ulogic_vector(4 downto 0);
    frm         : std_ulogic_vector(2 downto 0);
    sup0        : std_ulogic_vector(31 downto 0);
    sup1        : std_ulogic_vector(31 downto 0);
    epc         : std_ulogic_vector(core_addr_width_c-1 downto 0);
    badvaddr    : std_ulogic_vector(31 downto 0);
    ptbr        : std_ulogic_vector(31 downto 0);
    asid        : std_ulogic_vector(31 downto 0);
    count       : unsigned(31 downto 0);
    compare     : unsigned(31 downto 0);
    evec        : std_ulogic_vector(core_addr_width_c-1 downto 0);
    cause       : std_ulogic_vector(4 downto 0);
    hartid      : std_ulogic_vector(31 downto 0);
    impl        : std_ulogic_vector(31 downto 0);
    fatc        : std_ulogic_vector(31 downto 0);
    irq_ipi     : std_ulogic;
    stats       : std_ulogic;
    reset       : std_ulogic_vector(31 downto 0);
    tohost      : std_ulogic_vector(31 downto 0);
    fromhost    : std_ulogic_vector(31 downto 0);
    time        : unsigned(31 downto 0); -- TODO: use more intelligent counter and change to 64 bit as specified
    instret     : unsigned(31 downto 0);
  end record csr_reg_t;
  constant dflt_csr_reg_c : csr_reg_t :=(
    status      => dflt_csr_status_c,
    fflags      => (others=>'0'),
    frm         => (others=>'0'),
    sup0        => (others=>'0'),
    sup1        => (others=>'0'),
    epc         => (others=>'0'),
    badvaddr    => (others=>'0'),
    ptbr        => (others=>'0'),
    asid        => (others=>'0'),
    count       => (others=>'0'),
    compare     => (others=>'0'),
    evec        => (others=>'0'),
    cause       => (others=>'0'),
    hartid      => (others=>'0'),
    impl        => (others=>'0'),
    fatc        => (others=>'0'),
    irq_ipi     => '0',
    stats       => '0',
    reset       => (others=>'0'),
    tohost      => (others=>'0'),
    fromhost    => (others=>'0'),
    time        => (others=>'0'),
    instret     => (others=>'0')
  );

  type irq_arr_t is array(natural range 0 to 7) of std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0); 

  type exeption_cause_t is record
    misaligned_fetch       : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    fault_fetch            : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    illegal_instruction    : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    privileged_instruction : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    fp_disabled            : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    syscall                : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    breakpoint             : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    misaligned_load        : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    misaligned_store       : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    fault_load             : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    fault_store            : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    accelerator_disabled   : std_ulogic_vector(dflt_csr_reg_c.cause'left downto 0);
    interrupt              : irq_arr_t;
  end record exeption_cause_t;
  constant exeption_casuse_c : exeption_cause_t :=(
    misaligned_fetch       => std_ulogic_vector(to_unsigned( 0, dflt_csr_reg_c.cause'length)),
    fault_fetch            => std_ulogic_vector(to_unsigned( 1, dflt_csr_reg_c.cause'length)),
    illegal_instruction    => std_ulogic_vector(to_unsigned( 2, dflt_csr_reg_c.cause'length)),
    privileged_instruction => std_ulogic_vector(to_unsigned( 3, dflt_csr_reg_c.cause'length)),
    fp_disabled            => std_ulogic_vector(to_unsigned( 4, dflt_csr_reg_c.cause'length)),
    syscall                => std_ulogic_vector(to_unsigned( 6, dflt_csr_reg_c.cause'length)),
    breakpoint             => std_ulogic_vector(to_unsigned( 7, dflt_csr_reg_c.cause'length)),
    misaligned_load        => std_ulogic_vector(to_unsigned( 8, dflt_csr_reg_c.cause'length)),
    misaligned_store       => std_ulogic_vector(to_unsigned( 9, dflt_csr_reg_c.cause'length)),
    fault_load             => std_ulogic_vector(to_unsigned(10, dflt_csr_reg_c.cause'length)),
    fault_store            => std_ulogic_vector(to_unsigned(11, dflt_csr_reg_c.cause'length)),
    accelerator_disabled   => std_ulogic_vector(to_unsigned(12, dflt_csr_reg_c.cause'length)),
    interrupt              => ( 0 => std_ulogic_vector(to_unsigned(16, dflt_csr_reg_c.cause'length)),
                                1 => std_ulogic_vector(to_unsigned(17, dflt_csr_reg_c.cause'length)),
                                2 => std_ulogic_vector(to_unsigned(18, dflt_csr_reg_c.cause'length)),
                                3 => std_ulogic_vector(to_unsigned(19, dflt_csr_reg_c.cause'length)),
                                4 => std_ulogic_vector(to_unsigned(20, dflt_csr_reg_c.cause'length)),
                                5 => std_ulogic_vector(to_unsigned(21, dflt_csr_reg_c.cause'length)),
                                6 => std_ulogic_vector(to_unsigned(22, dflt_csr_reg_c.cause'length)),
                                7 => std_ulogic_vector(to_unsigned(23, dflt_csr_reg_c.cause'length)))
  );


  function get_opcode(inst : std_ulogic_vector(31 downto 0)) return std_ulogic_vector;
  function get_rega(inst : std_ulogic_vector(31 downto 0)) return std_ulogic_vector;
  function get_regb(inst : std_ulogic_vector(31 downto 0)) return std_ulogic_vector;
  function get_regd(inst : std_ulogic_vector(31 downto 0)) return std_ulogic_vector;

  function get_inst(inst : std_ulogic_vector(31 downto 0)) return inst_rtype_t;
  function get_inst(inst : std_ulogic_vector(31 downto 0)) return inst_itype_t;
  function get_inst(inst : std_ulogic_vector(31 downto 0)) return inst_stype_t;
  function get_inst(inst : std_ulogic_vector(31 downto 0)) return inst_utype_t;

  function get_iimm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector;
  function get_simm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector;
  function get_bimm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector;
  function get_uimm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector;
  function get_jimm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector;

  function csr_status(status : csr_status_t) return std_ulogic_vector;
  function csr_status(status : std_ulogic_vector(31 downto 0)) return csr_status_t;
  function csr_priviledged(csr_addr : std_ulogic_vector(3 downto 0); csr_wr : std_ulogic; mode : std_ulogic) return std_ulogic;

end package inst_pkg;

package body inst_pkg is

  function get_opcode(inst : std_ulogic_vector(31 downto 0)) return std_ulogic_vector is
  begin
    return inst(6 downto 0);
  end function get_opcode;

  function get_rega(inst : std_ulogic_vector(31 downto 0)) return std_ulogic_vector is
  begin
    return inst(19 downto 15);
  end function get_rega;
  
  function get_regb(inst : std_ulogic_vector(31 downto 0)) return std_ulogic_vector is
  begin
    return inst(24 downto 20);
  end function get_regb;

  function get_regd(inst : std_ulogic_vector(31 downto 0)) return std_ulogic_vector is
  begin
    return inst(11 downto 7);
  end function get_regd;

  function get_inst(inst : std_ulogic_vector(31 downto 0)) return inst_rtype_t is
    variable v : inst_rtype_t;
  begin
    v.opcode := inst( 6 downto  0);
    v.rd     := inst(11 downto  7);
    v.func3  := inst(14 downto 12);
    v.rs1    := inst(19 downto 15);
    v.rs2    := inst(24 downto 20);
    v.func7  := inst(31 downto 25);
    return v;
  end function get_inst;

  function get_inst(inst : std_ulogic_vector(31 downto 0)) return inst_itype_t is
    variable v : inst_itype_t;
  begin
    v.opcode := inst( 6 downto  0);
    v.rd     := inst(11 downto  7);
    v.func3  := inst(14 downto 12);
    v.rs1    := inst(19 downto 15);
    v.imm12  := inst(31 downto 20);
    return v;
  end function get_inst;

  function get_inst(inst : std_ulogic_vector(31 downto 0)) return inst_stype_t is
    variable v : inst_stype_t;
  begin
    v.opcode := inst( 6 downto  0);
    v.imm5   := inst(11 downto  7);
    v.func3  := inst(14 downto 12);
    v.rs1    := inst(19 downto 15);
    v.rs2    := inst(24 downto 20);
    v.imm7   := inst(31 downto 25);
    return v;
  end function get_inst;  

  function get_inst(inst : std_ulogic_vector(31 downto 0)) return inst_utype_t is
    variable v : inst_utype_t;
  begin
    v.opcode := inst( 6 downto  0);
    v.rd     := inst(11 downto  7);
    v.imm20  := inst(31 downto 12);
    return v;
  end function get_inst;

  function get_iimm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector is
    variable imm : std_ulogic_vector(31 downto 0);
  begin
    imm(10 downto  0) := inst(30 downto 20);
    imm(31 downto 11) := (others=>inst(31));
    return imm;
  end function get_iimm;

  function get_simm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector is
    variable imm : std_ulogic_vector(31 downto 0);
  begin
    imm( 4 downto  0) := inst(11 downto  7);
    imm(10 downto  5) := inst(30 downto 25);
    imm(31 downto 11) := (others=>inst(31));
    return imm;
  end function get_simm;

  function get_bimm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector is
    variable imm : std_ulogic_vector(31 downto 0);
  begin
    imm(0)            := '0';
    imm (4 downto  1) := inst(11 downto  8);
    imm(10 downto  5) := inst(30 downto 25);
    imm(11)           := inst(7);
    imm(31 downto 12) := (others=>inst(31));
    return imm;
  end function get_bimm;

  function get_uimm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector is
    variable imm : std_ulogic_vector(31 downto 0);
  begin
    imm(12 downto  0) := (others=>'0');
    imm(31 downto 12) := inst(31 downto 12);
    return imm;
  end function get_uimm;

  function get_jimm(inst :std_ulogic_vector(31 downto 0) ) return std_ulogic_vector is
    variable imm : std_ulogic_vector(31 downto 0);
  begin
    imm(0)            := '0';
    imm(10 downto  1) := inst(30 downto 21);
    imm(11)           := inst(20);
    imm(19 downto 12) := inst(19 downto 12);
    imm(31 downto 20) := (others=>inst(31));
    return imm;
  end function get_jimm;

  function csr_status(status : csr_status_t) return std_ulogic_vector is
    variable result : std_ulogic_vector(31 downto 0);
  begin
    result := status.ip & status.im & (15 downto 9 => '0') & status.er & status.vm & status.s64 & 
              status.u64 & status.ef & status.pei & status.ei & status.ps & status.s;
    return result;
  end function csr_status;

  function csr_status(status : std_ulogic_vector(31 downto 0)) return csr_status_t is
    variable result : csr_status_t;
  begin
    result.s   := status(0);
    result.ps  := status(1);
    result.ei  := status(2);
    result.pei := status(3);
    result.ef  := status(4);
    result.u64 := status(5);
    result.s64 := status(6);
    result.vm  := status(7);
    result.er  := status(8);
    result.im  := status(23 downto 16);
    result.ip  := status(31 downto 24);
    return result;
  end function csr_status;

  function csr_priviledged(csr_addr : std_ulogic_vector(3 downto 0); csr_wr : std_ulogic; mode : std_ulogic) return std_ulogic is
    variable priviledged : std_ulogic;
  begin
     priviledged := ( compare(csr_addr(3 downto 2), "11") and csr_wr   ) or
                    ( compare(csr_addr(3 downto 2), "10")              ) or
                    ( compare(csr_addr(3 downto 2), "01") and not mode ) or
                    ( compare(csr_addr(1 downto 0), "10") or compare(csr_addr(1 downto 0), "11") ) or
                    ( compare(csr_addr(1 downto 0), "01") and not mode and csr_wr );                
    return priviledged;
  end function csr_priviledged;

end package body inst_pkg;

