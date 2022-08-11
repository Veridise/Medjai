%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin, BitwiseBuiltin
from starkware.cairo.common.uint256 import (
    Uint256,
    uint256_add,
    uint256_sub,
    uint256_mul,
    uint256_eq,
    uint256_le,
    uint256_check,
)
from starkware.cairo.common.math import assert_not_equal, assert_le_felt
from starkware.cairo.common.math_cmp import is_not_zero, is_nn
from starkware.starknet.common.syscalls import get_caller_address
from contracts.safe_math import add, _add, sub, _sub, mul, _mul
from contracts.assertions import (
    assert_either,
    either,
    both,
    assert_both,
    not_0,
    assert_not_0,
    assert_0,
    ge,
    ge_0,
    le,
    assert_le,
    le_0,
    eq_0,
)

# Solidity code based on: https://github.com/makerdao/xdomain-dss/commit/5e91f8fbea66200f29037f4dcc4065a4062eb14f

# // --- Data ---
# mapping (address => uint256) public wards;
@storage_var
func _wards(user : felt) -> (res : felt):
end

# mapping(address => mapping (address => uint256)) public can;
@storage_var
func _can(b : felt, u : felt) -> (res : felt):
end

# struct Ilk {
#     uint256 Art;   // Total Normalised Debt     [wad]
#     uint256 rate;  // Accumulated Rates         [ray]
#     uint256 spot;  // Price with Safety Margin  [ray]
#     uint256 line;  // Debt Ceiling              [rad]
#     uint256 dust;  // Urn Debt Floor            [rad]
# }
struct Ilk:
    member Art : Uint256  # Total Normalised Debt     [wad]
    member rate : Uint256  # Accumulated Rates         [ray]
    member spot : Uint256  # Price with Safety Margin  [ray]
    member line : Uint256  # Debt Ceiling              [rad]
    member dust : Uint256  # Urn Debt Floor            [rad]
end

# struct Urn {
#   uint256 ink;   // Locked Collateral  [wad]
#   uint256 art;   // Normalised Debt    [wad]
# }
struct Urn:
    member ink : Uint256  # Locked Collateral  [wad]
    member art : Uint256  # Normalised Debt    [wad]
end

# mapping (bytes32 => Ilk)                       public ilks;
@storage_var
func _ilks(i : felt) -> (ilk : Ilk):
end

# mapping (bytes32 => mapping (address => Urn )) public urns;
@storage_var
func _urns(i : felt, u : felt) -> (urn : Urn):
end

# mapping (bytes32 => mapping (address => uint)) public gem;  // [wad]
@storage_var
func _gem(i : felt, u : felt) -> (gem : Uint256):
end

# mapping (address => uint256)                   public dai;  // [rad]
@storage_var
func _dai(u : felt) -> (dai : Uint256):
end

# mapping (address => uint256)                   public sin;  // [rad]
@storage_var
func _sin(u : felt) -> (sin : Uint256):
end

# uint256 public debt;  // Total Dai Issued    [rad]
@storage_var
func _debt() -> (debt : Uint256):
end

# uint256 public vice;  // Total Unbacked Dai  [rad]
@storage_var
func _vice() -> (vice : Uint256):
end

# uint256 public Line;  // Total Debt Ceiling  [rad]
@storage_var
func _Line() -> (Line : Uint256):
end

# uint256 public live;  // Active Flag
@storage_var
func _live() -> (live : felt):
end

# views
@view
func wards{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(user : felt) -> (
    res : felt
):
    let (res) = _wards.read(user)
    return (res)
end

@view
func can{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(b : felt, u : felt) -> (
    res : felt
):
    let (res) = _can.read(b, u)
    return (res)
end

@view
func ilks{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(i : felt) -> (
    ilk : Ilk
):
    let (ilk) = _ilks.read(i)
    return (ilk)
end

@view
func urns{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    i : felt, u : felt
) -> (urn : Urn):
    let (urn) = _urns.read(i, u)
    return (urn)
end

@view
func dai{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(u : felt) -> (
    dai : Uint256
):
    let (dai) = _dai.read(u)
    return (dai)
end

# TODO: views
# gem
# sin
# debt
# vice
# Line
# live

# // --- Events ---
# event Rely(address indexed usr);
@event
func Rely(user : felt):
end

# event Deny(address indexed usr);
@event
func Deny(user : felt):
end

# event Init(bytes32 indexed ilk);
# event File(bytes32 indexed what, uint256 data);
@event
func File(what : felt, data : Uint256):
end

# event File(bytes32 indexed ilk, bytes32 indexed what, uint256 data);
@event
func File_ilk(ilk : felt, what : felt, data : Uint256):
end

# event Cage();
# event Hope(address indexed from, address indexed to);
# event Nope(address indexed from, address indexed to);
# event Slip(bytes32 indexed ilk, address indexed usr, int256 wad);
# event Flux(bytes32 indexed ilk, address indexed src, address indexed dst, uint256 wad);
# event Move(address indexed src, address indexed dst, uint256 rad);
# event Frob(bytes32 indexed i, address indexed u, address v, address w, int256 dink, int256 dart);
# event Fork(bytes32 indexed ilk, address indexed src, address indexed dst, int256 dink, int256 dart);
# event Grab(bytes32 indexed i, address indexed u, address v, address w, int256 dink, int256 dart);
# event Heal(address indexed u, uint256 rad);
# event Suck(address indexed u, address indexed v, uint256 rad);
# event Fold(bytes32 indexed i, address indexed u, int256 rate);

# modifier auth {
#     require(wards[msg.sender] == 1, "Vat/not-authorized");
#     _;
# }
func auth{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    let (caller) = get_caller_address()
    let (ward) = _wards.read(caller)
    with_attr error_message("l2_dai_bridge/not-authorized"):
        assert ward = 1
    end
    return ()
end

# function wish(address bit, address usr) internal view returns (bool) {
#     return either(bit == usr, can[bit][usr] == 1);
# }
@external
func wish{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    bit : felt, usr : felt
) -> (res : felt):
    # return either(bit == usr, can[bit][usr] == 1);
    if bit == usr:
        return (res=1)
    end
    let (res) = _can.read(bit, usr)
    return (res)
end

# // --- Init ---
# constructor() {
@constructor
func constructor{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(ward : felt):
    # wards[msg.sender] = 1;
    _wards.write(ward, 1)

    # live = 1;
    _live.write(1)

    # emit Rely(msg.sender);
    Rely.emit(ward)

    return ()
end

# // --- Math ---
# function _add(uint256 x, int256 y) internal pure returns (uint256 z) {
#     unchecked {
#         z = x + uint256(y);
#     }
#     require(y >= 0 || z <= x);
#     require(y <= 0 || z >= x);
# }

# function _sub(uint256 x, int256 y) internal pure returns (uint256 z) {
#     unchecked {
#         z = x - uint256(y);
#     }
#     require(y <= 0 || z <= x);
#     require(y >= 0 || z >= x);
# }

# function _int256(uint256 x) internal pure returns (int256 y) {
#     require((y = int256(x)) >= 0);
# }

func require_live{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    # require(live == 1, "Vat/not-live");
    with_attr error_message("Vat/not-live"):
        let (live) = _live.read()
        assert live = 1
    end

    return ()
end

# // --- Administration ---
# function rely(address usr) external auth {
@external
func rely{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(usr : felt):
    auth()

    # require(live == 1, "Vat/not-live");
    require_live()

    # wards[usr] = 1;
    _wards.write(usr, 1)

    # emit Rely(usr);
    Rely.emit(usr)

    return ()
end



# function deny(address usr) external auth {
@external
func deny{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(user : felt):
    auth()

    # require(live == 1, "Vat/not-live");
    # TODO: consider: https://github.com/makerdao/xdomain-dss/issues/4
    require_live()

    # wards[usr] = 0;
    _wards.write(user, 0)

    # emit Deny(usr);
    Deny.emit(user)

    return ()
end

# function init(bytes32 ilk) external auth {
# TODO: consider: https://github.com/makerdao/xdomain-dss/issues/2
@external
func init{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(ilk : felt):
    alloc_locals

    auth()

    # require(ilks[ilk].rate == 0, "Vat/ilk-already-init");
    # ilks[ilk].rate = 10 ** 27;
    let (local i) = _ilks.read(ilk)  # TODO: is local necessary
    with_attr error_message("Vat/ilk-already-init"):
        assert_0(i.rate)
    end
    _ilks.write(
        ilk,
        Ilk(Art=i.Art, rate=Uint256(low=10 ** 27, high=0), spot=i.spot, line=i.line, dust=i.dust),
    )

    # TODO:
    #     emit Init(ilk);

    return ()
end

# function file(bytes32 what, uint256 data) external auth {
@external
func file{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    what : felt, data : Uint256
):
    auth()

    # require(live == 1, "Vat/not-live");
    require_live()

    # if (what == "Line") Line = data;
    # else revert("Vat/file-unrecognized-param");
    with_attr error_message("Vat/file-unrecognized-param"):
        assert what = 'Line'
    end

    _Line.write(data)

    # TODO
    # emit File(what, data);
    File.emit(what, data)

    return ()
end

# function file(bytes32 ilk, bytes32 what, uint256 data) external auth {
@external
func file_ilk{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    ilk : felt, what : felt, data : Uint256
):
    alloc_locals

    auth()

    # require(live == 1, "Vat/not-live");
    require_live()

    let (local i) = _ilks.read(ilk)

    # if (what == "spot") ilks[ilk].spot = data;
    if what == 'spot':
        _ilks.write(ilk, Ilk(Art=i.Art, rate=i.rate, spot=data, line=i.line, dust=i.dust))
        return ()
    end

    # else if (what == "line") ilks[ilk].line = data;
    if what == 'line':
        _ilks.write(ilk, Ilk(Art=i.Art, rate=i.rate, spot=i.spot, line=data, dust=i.dust))
        return ()
    end

    # else if (what == "dust") ilks[ilk].dust = data;
    if what == 'dust':
        _ilks.write(ilk, Ilk(Art=i.Art, rate=i.rate, spot=i.spot, line=i.line, dust=data))
        return ()
    end

    # else revert("Vat/file-unrecognized-param");
    with_attr error_message("Vat/file-unrecognized-param"):
        assert 1 = 0
    end

    # TODO
    # emit File(ilk, what, data);
    File_ilk.emit(ilk, what, data)

    return ()
end

# function cage() external auth {
@external
func cage{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    # TODO
    auth()

    # live = 0;
    _live.write(0)

    # TODO
    # emit Cage();

    return ()
end

# TODO: not sure if getters make sense in Starknet?
# // --- Structs getters ---
# function Art(bytes32 ilk) external view returns (uint256 Art_) {
#     Art_ = ilks[ilk].Art;
# }

# function rate(bytes32 ilk) external view returns (uint256 rate_) {
#     rate_ = ilks[ilk].rate;
# }

# function spot(bytes32 ilk) external view returns (uint256 spot_) {
#     spot_ = ilks[ilk].spot;
# }

# function line(bytes32 ilk) external view returns (uint256 line_) {
#     line_ = ilks[ilk].line;
# }

# function dust(bytes32 ilk) external view returns (uint256 dust_) {
#     dust_ = ilks[ilk].dust;
# }

# function ink(bytes32 ilk, address urn) external view returns (uint256 ink_) {
#     ink_ = urns[ilk][urn].ink;
# }
@view
func ink{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(i : felt, u : felt) -> (
    res : Uint256
):
    let (res : Urn) = _urns.read(i, u)
    return (res.ink)
end

@view
func gem{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(i : felt, u : felt) -> (
    res : Uint256
):
    let (res : Uint256) = _gem.read(i, u)
    return (res)
end

# function art(bytes32 ilk, address urn) external view returns (uint256 art_) {
#     art_ = urns[ilk][urn].art;
# }

# // --- Allowance ---
# function hope(address usr) external {

@external
func hope{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(usr : felt):
    # can[msg.sender][usr] = 1;
    let (caller) = get_caller_address()
    _can.write(caller, usr, 1)

    # TODO:
    # emit Hope(msg.sender, usr);

    return ()
end

# function nope(address usr) external {
@external
func nope{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(usr : felt):
    # can[msg.sender][usr] = 0;
    let (caller) = get_caller_address()
    _can.write(caller, usr, 0)

    # TODO:
    # emit Nope(msg.sender, usr);

    return ()
end

func check{range_check_ptr}(a : Uint256):
    with_attr error_message("Vat/invalid amount"):
        uint256_check(a)
    end
    return ()
end

# // --- Fungibility ---
# function slip(bytes32 ilk, address usr, int256 wad) external auth {
@external
func slip{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(ilk : felt, usr : felt, wad : Uint256):
    alloc_locals

    auth()

    check(wad)

    # gem[ilk][usr] = _add(gem[ilk][usr], wad);
    let (gem) = _gem.read(ilk, usr)
    let (gem) = _add(gem, wad)
    _gem.write(ilk, usr, gem)

    # TODO
    # emit Slip(ilk, usr, wad);

    return ()
end

# function flux(bytes32 ilk, address src, address dst, uint256 wad) external {
@external
func flux{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(ilk : felt, src : felt, dst : felt, wad : Uint256):
    alloc_locals

    check(wad)

    # require(wish(src, msg.sender), "Vat/not-allowed");
    let (caller) = get_caller_address()
    let (src_consents) = wish(src, caller)
    assert src_consents = 1

    # gem[ilk][src] = gem[ilk][src] - wad;
    let (gem_src) = _gem.read(ilk, src)
    let (gem_src) = sub(gem_src, wad)
    _gem.write(ilk, src, gem_src)

    # gem[ilk][dst] = gem[ilk][dst] + wad;
    let (gem_dst) = _gem.read(ilk, dst)
    let (gem_dst) = add(gem_dst, wad)
    _gem.write(ilk, dst, gem_dst)

    # TODO
    # emit Flux(ilk, src, dst, wad);

    return ()
end

# function move(address src, address dst, uint256 rad) external {
@external
func move{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(src : felt, dst : felt, rad : Uint256):
    alloc_locals

    # check(rad)

    # require(wish(src, msg.sender), "Vat/not-allowed");
    let (caller) = get_caller_address()
    # let caller = 0
    let (src_consents) = wish(src, caller)
    with_attr error_message("Vat/not-allowed"):
        assert src_consents = 1
    end

    # dai[src] = dai[src] - rad;
    let (dai_src) = _dai.read(src)
    # DEMO: replace sub with uint256_sub to cause a bug
    # let (local res) = uint256_le(rad, dai_src)
    # assert res = 1
    let (dai_src) = sub(dai_src, rad)
    _dai.write(src, dai_src)

    # dai[dst] = dai[dst] + rad;
    let (dai_dst) = _dai.read(dst)
    let (dai_dst) = add(dai_dst, rad)
    _dai.write(dst, dai_dst)

    # TODO
    # emit Move(src, dst, rad);

    return ()
end

# Helpers
# function either(bool x, bool y) internal pure returns (bool z) {
#     assembly{ z := or(x, y)}
# }

# function both(bool x, bool y) internal pure returns (bool z) {
#     assembly{ z := and(x, y)}
# }

# // --- CDP Manipulation ---
# function frob(bytes32 i, address u, address v, address w, int256 dink, int256 dart) external {
@external
func frob{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(i : felt, u : felt, v : felt, w : felt, dink : Uint256, dart : Uint256):
    alloc_locals

    check(dink)
    check(dart)

    # // system is live
    # require(live == 1, "Vat/not-live");
    require_live()

    # Urn memory urn = urns[i][u];
    # Ilk memory ilk = ilks[i];
    let (urn) = _urns.read(i, u)
    let (local ilk) = _ilks.read(i)

    # // ilk has been initialised
    # require(ilk.rate != 0, "Vat/ilk-not-init");
    with_attr error_message("Vat/ilk-not-init"):
        assert_not_0(ilk.rate)
    end

    # urn.ink = _add(urn.ink, dink);
    # urn.art = _add(urn.art, dart);
    # ilk.Art = _add(ilk.Art, dart);
    let (ink) = _add(urn.ink, dink)
    let (art) = _add(urn.art, dart)
    _urns.write(i, u, Urn(ink, art))
    let (Art) = _add(ilk.Art, dart)
    _ilks.write(i, Ilk(Art, ilk.rate, ilk.spot, ilk.line, ilk.dust))

    # int256 dtab = _int256(ilk.rate) * dart;
    # uint256 tab = ilk.rate * urn.art;
    # debt     = _add(debt, dtab);
    let (dtab) = _mul(ilk.rate, dart)
    let (tab) = mul(ilk.rate, art)
    # let (tab)  = _mul(ilk.rate, art) COMMENT: both ilk.rate and art are unsinged, so above should work
    let (debt) = _debt.read()
    let (debt) = _add(debt, dtab)
    _debt.write(debt)

    # // either debt has decreased, or debt ceilings are not exceeded
    # require(either(dart <= 0, both(ilk.Art * ilk.rate <= ilk.line, debt <= Line)), "Vat/ceiling-exceeded");
    with_attr error_message("Vat/ceiling-exceeded"):
        let (debt_decreased) = le_0(dart)
        let (ilk_debt) = mul(Art, ilk.rate)
        # let (ilk_debt) = _mul(ilk.rate, Art) COMMENT: both ilk.rate and art are unsinged, so above should work
        let (line_ok) = le(ilk_debt, ilk.line)
        let (Line_ok) = le(debt, ilk.line)
        let (lines_ok) = both(line_ok, Line_ok)
        assert_either(debt_decreased, lines_ok)
    end

    # // urn is either less risky than before, or it is safe
    # require(either(both(dart <= 0, dink >= 0), tab <= urn.ink * ilk.spot), "Vat/not-safe");
    with_attr error_message("Vat/not-safe"):
        let (dart_le_0) = le_0(dart)
        let (dink_ge_0) = ge_0(dink)
        let (less_risky) = both(dart_le_0, dink_ge_0)
        let (brim) = mul(ink, ilk.spot)
        let (safe) = le(tab, brim)
        assert_either(less_risky, safe)
    end

    let (caller) = get_caller_address()

    # // urn is either more safe, or the owner consents
    # require(either(both(dart <= 0, dink >= 0), wish(u, msg.sender)), "Vat/not-allowed-u");
    with_attr error_message("Vat/not-allowed-u"):
        let (dart_le_0) = le_0(dart)
        let (dink_ge_0) = ge_0(dink)
        let (less_risky) = both(dart_le_0, dink_ge_0)
        let (owner_consents) = wish(u, caller)
        assert_either(less_risky, owner_consents)
    end

    # // collateral src consents
    # require(either(dink <= 0, wish(v, msg.sender)), "Vat/not-allowed-v");
    with_attr error_message("Vat/not-allowed-v"):
        let (dink_le_0) = le_0(dink)
        let (src_consents) = wish(v, caller)
        assert_either(dink_le_0, src_consents)
    end

    # // debt dst consents
    # require(either(dart >= 0, wish(w, msg.sender)), "Vat/not-allowed-w");
    with_attr error_message("Vat/not-allowed-w"):
        let (dart_ge_0) = ge_0(dart)
        let (dst_consents) = wish(w, caller)
        assert_either(dart_ge_0, dst_consents)
    end

    # // urn has no debt, or a non-dusty amount
    # require(either(urn.art == 0, tab >= ilk.dust), "Vat/dust");
    # TODO: how to manage underwater dusty vaults?
    with_attr error_message("Vat/dust"):
        let (no_debt) = eq_0(art)
        let (non_dusty) = ge(tab, ilk.dust)
        assert_either(no_debt, non_dusty)
    end

    # gem[i][v] = sub(gem[i][v], dink);
    let (gem) = _gem.read(i, v)
    let (gem) = _sub(gem, dink)
    _gem.write(i, v, gem)

    # dai[w]    = add(dai[w],    dtab);
    let (dai) = _dai.read(w)
    let (dai) = _add(dai, dtab)
    _dai.write(w, dai)

    # urns[i][u] = urn;
    _urns.write(i, u, Urn(ink, art))

    # ilks[i]    = ilk;
    _ilks.write(i, Ilk(Art=Art, rate=ilk.rate, spot=ilk.spot, line=ilk.line, dust=ilk.dust))

    # TODO
    # emit Frob(i, u, v, w, dink, dart);

    return ()
end

# // --- CDP Fungibility ---
# function fork(bytes32 ilk, address src, address dst, int256 dink, int256 dart) external {
@external
func fork{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(ilk : felt, src : felt, dst : felt, dink : Uint256, dart : Uint256):
    alloc_locals

    check(dink)
    check(dart)

    # Urn storage u = urns[ilk][src];
    # Urn storage v = urns[ilk][dst];
    # Ilk storage i = ilks[ilk];
    let (u) = _urns.read(ilk, src)
    let (v) = _urns.read(ilk, dst)
    let (i) = _ilks.read(ilk)

    # u.ink = _sub(u.ink, dink);
    # u.art = _sub(u.art, dart);
    # v.ink = _add(v.ink, dink);
    # v.art = _add(v.art, dart);
    let (u_ink) = _sub(u.ink, dink)
    let (u_art) = _sub(u.art, dart)
    let (v_ink) = _add(v.ink, dink)
    let (v_art) = _add(v.art, dart)

    _urns.write(ilk, src, Urn(ink=u_ink, art=u_art))
    _urns.write(ilk, dst, Urn(ink=v_ink, art=v_art))

    # uint256 utab = u.art * i.rate;
    # uint256 vtab = v.art * i.rate;
    let (u_tab) = mul(u_art, i.rate)
    let (v_tab) = mul(v_art, i.rate)

    let (caller) = get_caller_address()

    # // both sides consent
    # require(both(wish(src, msg.sender), wish(dst, msg.sender)), "Vat/not-allowed");
    with_attr error_message("Vat/not-allowed"):
        let (src_consents) = wish(src, caller)
        let (dst_consents) = wish(dst, caller)
        assert_both(src_consents, dst_consents)
    end

    # // both sides safe
    # require(utab <= u.ink * i.spot, "Vat/not-safe-src")
    with_attr error_message("Vat/not-safe-src"):
        let (brim) = mul(u_ink, i.spot)
        assert_le(u_tab, brim)
    end
    # require(vtab <= v.ink * i.spot, "Vat/not-safe-dst");
    with_attr error_message("Vat/not-safe-dst"):
        let (brim) = mul(v_ink, i.spot)
        assert_le(v_tab, brim)
    end

    # // both sides non-dusty
    # require(either(utab >= i.dust, u.art == 0), "Vat/dust-src");
    with_attr error_message("Vat/dust-src"):
        let (u_tab_le_i_dust) = ge(u_tab, i.dust)
        let (u_art_eq_0) = eq_0(u_art)
        assert_either(u_tab_le_i_dust, u_art_eq_0)
    end

    # require(either(vtab >= i.dust, v.art == 0), "Vat/dust-dst");
    with_attr error_message("Vat/dust-dst"):
        let (v_tab_le_i_dust) = ge(v_tab, i.dust)
        let (v_art_eq_0) = eq_0(v_art)
        assert_either(v_tab_le_i_dust, v_art_eq_0)
    end

    # TODO
    # emit Fork(ilk, src, dst, dink, dart);

    return ()
end

# // --- CDP Confiscation ---
# function grab(bytes32 i, address u, address v, address w, int256 dink, int256 dart) external auth {
@external
func grab{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(i : felt, u : felt, v : felt, w : felt, dink : Uint256, dart : Uint256):
    alloc_locals

    auth()

    check(dink)
    check(dart)

    # Urn storage urn = urns[i][u];
    # Ilk storage ilk = ilks[i];
    let (urn) = _urns.read(i, u)
    let (ilk) = _ilks.read(i)

    # urn.ink = _add(urn.ink, dink);
    # urn.art = _add(urn.art, dart);
    let (ink) = _add(urn.ink, dink)
    let (art) = _add(urn.art, dart)
    _urns.write(i, u, Urn(ink=ink, art=art))

    # ilk.Art = _add(ilk.Art, dart);
    let (Art) = _add(ilk.Art, dart)
    _ilks.write(i, Ilk(Art=Art, rate=ilk.rate, spot=ilk.spot, line=ilk.line, dust=ilk.dust))

    # int256 dtab = _int256(ilk.rate) * dart;
    # let (dtab) = _mul(ilk.rate, dart)
    let dtab = Uint256(low=0, high=0)
    # gem[i][v] = _sub(gem[i][v], dink);
    let (gem) = _gem.read(i, v)
    let (gem) = _sub(gem, dink)
    _gem.write(i, v, gem)

    # sin[w]    = _sub(sin[w],    dtab);
    let (sin) = _sin.read(w)
    let (sin) = _sub(sin, dtab)
    _sin.write(w, sin)

    # vice      = _sub(vice,      dtab);
    let (vice) = _vice.read()
    let (vice) = _sub(vice, dtab)
    _vice.write(vice)

    # TODO
    # emit Grab(i, u, v, w, dink, dart);

    return ()
end

# // --- Settlement ---
# function heal(uint256 rad) external {
@external
func heal{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(rad : Uint256):
    alloc_locals

    check(rad)

    # address u = msg.sender;
    let (u) = get_caller_address()

    # sin[u] = sin[u] - rad;
    let (sin) = _sin.read(u)
    let (sin) = sub(sin, rad)
    _sin.write(u, sin)

    # dai[u] = dai[u] - rad;
    let (dai) = _dai.read(u)
    let (dai) = sub(dai, rad)
    _dai.write(u, dai)

    # vice   = vice   - rad;
    let (vice) = _vice.read()
    let (vice) = sub(vice, rad)
    _vice.write(vice)

    # debt   = debt   - rad;
    let (debt) = _debt.read()
    let (debt) = sub(debt, rad)
    _debt.write(debt)

    # TODO
    # emit Heal(msg.sender, rad);

    return ()
end

# function suck(address u, address v, uint256 rad) external auth {
@external
func suck{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(u : felt, v : felt, rad : Uint256):
    alloc_locals

    auth()

    check(rad)

    # sin[u] = sin[u] + rad;
    let (local sin) = _sin.read(u)
    let (local sin) = sub(sin, rad)
    _sin.write(u, sin)

    # TODO
    # dai[v] = dai[v] + rad;
    let (local dai) = _dai.read(v)
    let (local dai) = sub(dai, rad)
    _dai.write(v, dai)

    # vice   = vice   + rad;
    let (vice) = _vice.read()
    let (vice) = sub(vice, rad)
    _vice.write(vice)

    # debt   = debt   + rad;
    let (debt) = _debt.read()
    let (debt) = sub(debt, rad)
    _debt.write(debt)

    # TODO
    # emit Suck(u, v, rad);

    return ()
end

# // --- Rates ---
# function fold(bytes32 i, address u, int256 rate_) external auth {
@external
func fold{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(i : felt, u : felt, rate : Uint256):
    alloc_locals

    auth()

    check(rate)

    # require(live == 1, "Vat/not-live");
    require_live()

    # Ilk storage ilk = ilks[i];
    let (ilk) = _ilks.read(i)

    # ilk.rate = _add(ilk.rate, rate_);
    let (ilk_rate) = _add(ilk.rate, rate)

    _ilks.write(i, Ilk(Art=ilk.Art, rate=ilk_rate, spot=ilk.spot, line=ilk.line, dust=ilk.dust))

    # int256 rad  = _int256(ilk.Art) * rate_;
    let (rad) = _mul(ilk.Art, rate)

    # dai[u]   = _add(dai[u], rad);
    let (dai) = _dai.read(u)
    let (dai) = add(dai, rad)
    _dai.write(u, dai)

    # debt     = _add(debt,   rad);
    let (debt) = _debt.read()
    let (debt) = add(debt, rad)
    _debt.write(debt)

    # TODO
    # emit Fold(i, u, rate_);
    return ()
end
