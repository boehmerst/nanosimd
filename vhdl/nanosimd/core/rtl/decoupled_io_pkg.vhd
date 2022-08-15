library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package decoupled_io_pkg is

  -- TODO: make package generic using VHDL-2008
  constant addr_width_short_c : integer := 12;
  constant data_width_short_c : integer := 32;
  constant addr_width_long_c  : integer := 21;
  constant data_width_long_c  : integer := 64;

  type bits_short_t is record
    rw   : std_ulogic;
    addr : std_ulogic_vector(addr_width_short_c-1 downto 0);
    data : std_ulogic_vector(data_width_short_c-1 downto 0);
  end record bits_short_t;
  constant dflt_bits_short_c : bits_short_t :=(
    rw   => '0',
    addr => (others=>'0'),
    data => (others=>'0')
  );

  type bits_long_t is record
    rw   : std_ulogic;
    addr : std_ulogic_vector(addr_width_long_c-1 downto 0);
    data : std_ulogic_vector(data_width_long_c-1 downto 0);
  end record bits_long_t;
  constant dflt_bits_long_c : bits_long_t :=(
    rw   => '0',
    addr => (others=>'0'),
    data => (others=>'0')
  );

  type decoupled_io_req_t is record
    valid : std_ulogic;
    bits  : bits_short_t;
  end record decoupled_io_req_t;
  constant dflt_decoupled_io_req_c : decoupled_io_req_t :=(
    valid => '0',
    bits  => dflt_bits_short_c
  );

  type decoupled_mem_req_t is record
    valid : std_ulogic;
    bits  : bits_long_t;
  end record decoupled_mem_req_t;
  constant dflt_decoupled_mem_req_c : decoupled_mem_req_t :=(
    valid => '0',
    bits  => dflt_bits_long_c
  );

  type decoupled_io_rsp_t is record
    ready : std_ulogic;
  end record decoupled_io_rsp_t;
  constant dflt_decoupled_io_rsp_c : decoupled_io_rsp_t :=(
    ready => '1'
  );

  subtype decoupled_mem_rsp_t is decoupled_io_rsp_t;
  constant dflt_decoupled_mem_rsp_c : decoupled_mem_rsp_t := dflt_decoupled_io_rsp_c;

  function io_fire(req : decoupled_io_req_t; rsp : decoupled_io_rsp_t) return std_ulogic;

end package decoupled_io_pkg;

package body decoupled_io_pkg is

  function io_fire(req : decoupled_io_req_t; rsp : decoupled_io_rsp_t) return std_ulogic is
  begin
    return req.valid and rsp.ready;
  end function io_fire;

end package body decoupled_io_pkg;

