library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.func_pkg.all;

package nanosimd_pkg is

  constant core_data_width_c : integer := 32;
  constant core_addr_width_c : integer := 32;

  type fetch_in_t is record
    stall             : std_ulogic;
    branch            : std_ulogic;
    branch_target     : std_ulogic_vector(core_addr_width_c-1 downto 0);
  end record fetch_in_t;

  type fetch_out_t is record
    pc                : std_ulogic_vector(core_addr_width_c-1 downto 0);
  end record fetch_out_t;

  --TODO These types are just a starting point to get things running
  --     We want decopubled memory access without requiring the core
  --     being stalled once the memory access gets delays
  --     so we want outstanding memory reads and prefetch buffers
  --     or maybe even caches
  type imem_in_t is record
    data              : std_ulogic_vector(core_addr_width_c-1 downto 0);
  end record imem_in_t;

  type imem_out_t is record
    addr              : std_ulogic_vector(core_addr_width_c-1 downto 0);
    en                : std_ulogic;
  end record imem_out_t;


  type dmem_in_t is record
    data              : std_ulogic_vector(core_data_width_c-1 downto 0);
  end record dmem_in_t;


  type dmem_out_t is record
    data              : std_ulogic_vector(core_data_width_c-1 downto 0);
    addr              : std_ulogic_vector(core_addr_width_c-1 downto 0);
    sel               : std_ulogic_vector(3 downto 0);
    we                : std_ulogic;
    en                : std_ulogic;
  end record dmem_out_t;
  constant dflt_dmem_out_c : dmem_out_t :=(
    data              => (others=>'0'),
    addr              => (others=>'0'),
    sel               => (others=>'0'),
    we                => '0',
    en                => '0'
  );











  
  type crash_dump_t is record
    current_pc     : std_ulogic_vector(core_addr_width_c-1 downto 0);
    next_pc        : std_ulogic_vector(core_addr_width_c-1 downto 0);
    last_data_addr : std_ulogic_vector(core_addr_width_c-1 downto 0); 
    exception_pc   : std_ulogic_vector(core_addr_width_c-1 downto 0);
    exception_addr : std_ulogic_vector(core_addr_width_c-1 downto 0);
  end record crash_dump_t;

  type core2rf_t is record
    dummy_instr_id : std_ulogic;
    raddr_a        : std_ulogic_vector(4 downto 0); 
    raddr_b        : std_ulogic_vector(4 downto 0);
    waddr_a        : std_ulogic_vector(4 downto 0);
    we_a           : std_ulogic;
  end record core2rf_t;
    
  type regfile_e is (RegFileFF, RegFileFPGA, RegFileLatch);
  type rv32m_e   is (RV32MNone, RV32MSlow, RV32MFast, RV32MSingleCycle);
  type opcode_e  is (OC_LOAD, OC_MISC_MEM, OC_OP_IMM, OC_AUIPC, OC_STORE, OC_OP, 
                     OC_LUI, OC_BRANCH, OC_JALR, OC_JAL, OC_SYSTEM);

  function opcode(instr : in std_ulogic_vector(6 downto 0)) return opcode_e;

  type alu_op_e is (ALU_ADD, ALU_SUB, ALU_XOR, ALU_OR, ALU_AND, ALU_SRA, ALU_SRL, ALU_SLL, 
                    ALU_LT, ALU_LTU, ALU_GE, ALU_GEU, ALU_EQ,ALU_NE);

  type md_op_e  is (MD_MULL, MD_MULH, MD_DIV, MD_REM);


end package nanosimd_pkg;

package body nanosimd_pkg is

  function opcode(instr : in std_ulogic_vector(6 downto 0)) return opcode_e is
    variable v : opcode_e;
  begin
    decode: case '0' & instr is
      when x"03"  => v := OC_LOAD;
      when x"0f"  => v := OC_MISC_MEM;
      when x"13"  => v := OC_OP_IMM;
      when x"17"  => v := OC_AUIPC;
      when x"23"  => v := OC_STORE;
      when x"33"  => v := OC_OP;
      when x"37"  => v := OC_LUI;
      when x"63"  => v := OC_BRANCH;
      when x"67"  => v := OC_JALR;
      when x"6f"  => v := OC_JAL;
      when x"73"  => v := OC_SYSTEM;
      when others => v := OC_OP;
    end case decode;
    return v;
  end function opcode;


end package body nanosimd_pkg;

