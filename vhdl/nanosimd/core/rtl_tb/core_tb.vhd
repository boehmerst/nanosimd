library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.func_pkg.all;
use work.nanosimd_pkg.all;
use work.config_pkg.all;
use work.decoupled_io_pkg.all;

entity core_tb is
end entity core_tb;

architecture beh of core_tb is

  signal sim_done      : boolean   := false;
  signal clk           : std_logic := '0';
  signal reset_n       : std_logic := '0';
  constant clk_cycle_c : time      := 10 ns;

  constant rom_size_c : integer    := 13;
  constant ram_size_c : integer    := 13;

  signal imem         : imem_in_t;
  signal dmem         : dmem_in_t;
  
  signal corei0_imem  : imem_out_t;
  signal corei0_dmem  : dmem_out_t;

begin
  ------------------------------------------------------------------------------
  -- clock and reset
  ------------------------------------------------------------------------------
  clk     <= not clk after clk_cycle_c/2 when sim_done = false;
  reset_n <= '1' after 500 ns;

  ------------------------------------------------------------------------------
  -- risc-v core
  ------------------------------------------------------------------------------
  corei0: entity work.core
    generic map (
      use_m_ext_g      => true
    )
    port map (
      clk_i            => clk,                       
      reset_n_i        => reset_n,
      init_i           => '0',
      irq_i            => '0',
      wait_n_i         => '1',
      imem_o           => corei0_imem,
      imem_i           => imem,
      dmem_o           => corei0_dmem,
      dmem_i           => dmem,
      receive_req_i    => dflt_decoupled_io_req_c,
      receive_rsp_o    => open,
      transmit_req_o   => open,
      transmit_rsp_i   => dflt_decoupled_io_rsp_c
    );

  ------------------------------------------------------------------------------
  -- 32 Bit instruction memory
  ------------------------------------------------------------------------------
  imemi0 : entity work.sram 
    generic map (
      file_name_g  => "./rom/rom.mem",
      data_width_g => core_data_width_c,
      addr_width_g => rom_size_c-2
    )
    port map (
      clk_i   => clk,
      we_i    => '0',
      en_i    => corei0_imem.en,
      addr_i  => corei0_imem.addr(rom_size_c-1 downto 2),
      di_i    => (others=>'0'),
      do_o    => imem.data
    );
    
  ------------------------------------------------------------------------------
  -- 32 Bit data memory
  ------------------------------------------------------------------------------
  dmemi0: block is
    signal mem_we    : std_ulogic_vector(3 downto 0);
    signal mem_en    : std_ulogic;
  begin   
    mem_we <= corei0_dmem.sel when corei0_dmem.we = '1' else "0000";
    mem_en <= corei0_dmem.en;
    
    memi0 : entity work.sram_4en
      generic map (
        file_name_g  => "./rom/rom.mem",
        data_width_g => core_data_width_c,
        addr_width_g => ram_size_c-2
      )
      port map (
        clk_i  => clk,
        we_i   => mem_we,
        en_i   => mem_en,
        addr_i => corei0_dmem.addr(ram_size_c-1 downto 2),
        di_i   => corei0_dmem.data,
        do_o   => dmem.data
      );
  end block dmemi0;


  ------------------------------------------------------------------------------
  -- stimuli
  ------------------------------------------------------------------------------
  stim0: process is
  begin
    wait until reset_n = '1';
    wait until rising_edge(clk);

    wait for 1 ms;

    sim_done <= true;
    assert false report "Simulation finished successfully!" severity failure;
    wait;
  end process stim0;
  
end architecture beh;
