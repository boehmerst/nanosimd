library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package config_pkg is

  constant core_data_width_c    : integer := 32;
  constant core_addr_width_c    : integer := 32;
  constant csr_addr_width_c     : integer := 12;

  -- TODO: make this generic to support RV32e
  constant gprf_size_c          : integer := 32;
  constant use_m_ext_c          : boolean := true;

  constant cfg_reg_force_zero_c : boolean := true;
  constant cfg_reg_fwd_wrb_c    : boolean := true;
  constant cfg_mem_fwd_wrb_c    : boolean := true;

end package config_pkg;

