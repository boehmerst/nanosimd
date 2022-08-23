library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.nanosimd_pkg.all;
use work.func_pkg.all;
use work.decoupled_io_pkg.all;

entity core is
  generic (
    use_m_ext_g    : boolean := true
  );
  port (
    clk_i          : in  std_ulogic;
    reset_n_i      : in  std_ulogic;
    init_i         : in  std_ulogic;
    irq_i          : in  std_ulogic;
    wait_n_i       : in  std_ulogic;
    imem_o         : out imem_out_t;
    imem_i         : in  imem_in_t;
    dmem_o         : out dmem_out_t;
    dmem_i         : in  dmem_in_t;
    receive_req_i  : in  decoupled_io_req_t;
    receive_rsp_o  : out decoupled_io_rsp_t;
    transmit_req_o : out decoupled_io_req_t;
    transmit_rsp_i : in  decoupled_io_rsp_t
  );
end entity core;

architecture rtl of core is

  signal en                   : std_ulogic;
  signal exec_en              : std_ulogic;

  signal fetch                : fetch_in_t;
  signal fetchi0_fetch        : fetch_out_t;

  signal exec                 : execute_in_t;
  signal execi0_execute       : execute_out_t;

begin
  ------------------------------------------------------------------------------
  -- enable for core and execution units
  ------------------------------------------------------------------------------
  en      <= wait_n_i; -- and execi0_ready;
  exec_en <= wait_n_i;

  ------------------------------------------------------------------------------
  -- fetch (IF)
  ------------------------------------------------------------------------------
  fetch.stall         <= '0';
  fetch.branch        <= execi0_execute.branch;
  fetch.branch_target <= execi0_execute.branch_target;
  
  fetchi0: entity work.ifetch
    generic map (
      fetch_addr_width_g => core_addr_width_c
    )
    port map (
      clk_i              => clk_i,
      reset_n_i          => reset_n_i,
      init_i             => init_i,
      en_i               => en,
      fetch_i            => fetch,
      fetch_o            => fetchi0_fetch,
      imem_o             => imem_o
    );

  -- TODO: some quick assignments to get simulation up
  exec.pc    <= fetchi0_fetch.pc;
  exec.inst  <= imem_i.data;
  exec.stall <= '0';

  execi0: entity work.iexecute
    generic map (
      exec_addr_width_g => core_addr_width_c,
      exec_data_width_g => core_data_width_c,
      exec_gprf_size_g  => gprf_size_c,
      use_m_ext_g       => use_m_ext_c
    )
    port map (
      clk_i              => clk_i,
      reset_n_i          => reset_n_i,
      init_i             => init_i,
      en_i               => exec_en,
      execute_i          => exec,
      execute_o          => execi0_execute
    );


end architecture rtl;

