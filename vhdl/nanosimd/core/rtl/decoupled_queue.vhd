library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.func_pkg.all;
use work.decoupled_io_pkg.all;

entity decoupled_queue is
  generic (
    entries_g : positive := 2;
    pipe_g    : boolean  := false;
    flow_g    : boolean  := false
  );
  port (
    clk_i     : in  std_ulogic;
    reset_n_i : in  std_ulogic;
    en_i      : in  std_ulogic;
    init_i    : in  std_ulogic;
    enq_req_i : in  decoupled_io_req_t;
    enq_rsp_o : out decoupled_io_rsp_t;
    deq_req_o : out decoupled_io_req_t;
    deq_rsp_i : in  decoupled_io_rsp_t
  );
end entity decoupled_queue;

architecture rtl of decoupled_queue is
  ------------------------------------------------------------------------------
  -- counter_inc
  ------------------------------------------------------------------------------
  function counter_inc(val : unsigned) return unsigned is
    variable wrap  : boolean;
    variable value : unsigned(val'range);
  begin
    if(entries_g = 1) then
      value := (others=>'0');
    else
      if(val = entries_g-1) then wrap := true; else wrap := false; end if; 

      if(not ispow2(entries_g) and wrap) then
        value := (others=>'0');
      else
        value := value + 1;
      end if;
    end if; 
    return value;
  end function counter_inc;

  type ram_t is array(natural range 0 to entries_g-1) of bits_short_t;
  constant dflt_ram_c : ram_t := (others => dflt_bits_short_c);

  type reg_t is record
    ram        : ram_t;
    enq_ptr    : unsigned(log2ceil(entries_g)-1 downto 0);
    deq_ptr    : unsigned(log2ceil(entries_g)-1 downto 0);
    maybe_full : std_ulogic;
  end record reg_t;
  constant dflt_reg_c : reg_t :=(
    ram => dflt_ram_c,
    enq_ptr    => (others=>'0'),
    deq_ptr    => (others=>'0'),
    maybe_full => '0'
  );

  signal r, rin : reg_t;

begin
  ------------------------------------------------------------------------------
  -- comb0
  ------------------------------------------------------------------------------
  comb0: process(r, enq_req_i, deq_rsp_i) is
    variable v          : reg_t;
    variable ptr_match  : std_ulogic;
    variable empty      : std_ulogic;
    variable full       : std_ulogic;
    variable maybe_flow : std_ulogic;
    variable do_flow    : std_ulogic;
    variable do_enq     : std_ulogic;
    variable do_deq     : std_ulogic;
    variable enq_rsp    : decoupled_io_rsp_t;
    variable deq_req    : decoupled_io_req_t;
  begin
    v := r;

    ptr_match    := '0';
    if(r.enq_ptr = r.deq_ptr) then
      ptr_match  := '1';
    end if;   
    
    empty        := ptr_match and not r.maybe_full;
    full         := ptr_match and     r.maybe_full;

    maybe_flow   := '0';
    if(flow_g) then
      maybe_flow := empty;
    end if;

    do_flow := maybe_flow      and deq_rsp_i.ready;
    do_enq  := enq_rsp.ready   and enq_req_i.valid and not do_flow; 
    do_deq  := deq_rsp_i.ready and deq_req.valid   and not do_flow;

    if(do_enq = '1') then
      v.ram(to_integer(r.enq_ptr)) := enq_req_i.bits;
      v.enq_ptr                    := counter_inc(r.enq_ptr);
    end if; 

    if(do_deq = '1') then
      v.deq_ptr                    := counter_inc(r.deq_ptr);
    end if;

    if(do_enq /= do_deq) then
      v.maybe_full := do_enq;
    end if;

    if(flow_g) then
      deq_req.valid := not empty or enq_req_i.valid;
    else
      deq_req.valid := not empty;
    end if;

    if(pipe_g) then
      enq_rsp.ready := not full or deq_rsp_i.ready;
    else
      enq_rsp.ready := not full;
    end if;

    if(maybe_flow = '1') then
      deq_req.bits := enq_req_i.bits;
    else
      deq_req.bits := r.ram(to_integer(r.deq_ptr));
    end if;

    ----------------------------------------------------------------------------
    -- drive module output
    ----------------------------------------------------------------------------
    deq_req_o <= deq_req;
    enq_rsp_o <= enq_rsp;

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

end architecture rtl;





