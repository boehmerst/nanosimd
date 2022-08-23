library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.nanosimd_pkg.all;

entity gprf_register is
  generic (
    dmem_width_g : positive := 32;
    gprf_size_g  : positive := 3
  );
  port (
    clk_i     : in  std_ulogic;
    reset_n_i : in  std_ulogic;
    gprf_i    : in  gprf_in_t;
    gprf_o    : out gprf_out_t
  );
end gprf_register;

architecture rtl of gprf_register is
  type regfile_t is array (natural range 0 to 2**gprf_size_g-1) of std_ulogic_vector(dmem_width_g-1 downto 0);

  -- NOTE: to save flops we could also register the addresses which are less and mux out the registerfile
  --       directely which would save flops at the cost of speed
  --       also, we could save R0
  type reg_t is record 
    regfile : regfile_t;
    dat_a   : std_ulogic_vector(dmem_width_g-1 downto 0);
    dat_b   : std_ulogic_vector(dmem_width_g-1 downto 0);
  end record reg_t;
  constant dflt_reg_c : reg_t :=(
    regfile => (others => (others => '0')),
    dat_a   => (others => '0'),
    dat_b   => (others => '0')
  );

  signal r, rin : reg_t;

begin
  -----------------------------------------------------------------------------
  -- comb0
  -----------------------------------------------------------------------------
  comb0: process (r, gprf_i) is
    variable v : reg_t;
  begin
    v := r;

    if gprf_i.wre = '1' then
      v.regfile(to_integer(unsigned(gprf_i.adr_w))) := gprf_i.dat_w;
    end if;
    
    v.dat_a := r.regfile(to_integer(unsigned(gprf_i.adr_a)));
    v.dat_b := r.regfile(to_integer(unsigned(gprf_i.adr_b)));

    gprf_o.dat_a <= r.dat_a;
    gprf_o.dat_b <= r.dat_b;

    rin <= v;
  end process comb0;
  

  -----------------------------------------------------------------------------
  -- sync0
  -----------------------------------------------------------------------------
  sync0: process (clk_i, reset_n_i) is
  begin
    if reset_n_i = '0' then
      r <= dflt_reg_c;
    elsif rising_edge(clk_i) then
      if gprf_i.ena = '1' then
	      r <= rin;
      end if;
    end if;
  end process sync0;

end architecture rtl;

