library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package config_pkg is

  constant core_data_width_c : integer := 32;
  constant core_addr_width_c : integer := 32;
  constant csr_addr_width_c  : integer := 12;

  -- TODO: make this generic to support RV32e
  constant gprf_size_c       : integer := 32;

end package config_pkg;

