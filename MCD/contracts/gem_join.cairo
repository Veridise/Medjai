# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2021 Dai Foundation
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.starknet.common.syscalls import (get_contract_address, get_caller_address)
from starkware.cairo.common.math import (
  assert_le,
)
from starkware.cairo.common.uint256 import (
  Uint256,
  uint256_mul,
  split_64,
  uint256_le
)

@contract_interface
namespace IGem:
  func decimals() -> (res : felt):
  end

  func transfer(to_address : felt, value : Uint256) -> (res: felt):
  end

  func transferFrom(from_address : felt, to_address : felt, value : Uint256) -> (res : felt):
  end
end

@contract_interface
namespace IVat:
  func slip(ilk: felt, usr: felt, wad: Uint256):
  end
end


# event Rely(address indexed usr);
# event Deny(address indexed usr);
# event Join(address indexed usr, uint256 wad);
# event Exit(address indexed usr, uint256 wad);
# event Cage();
@event
func Rely(user : felt):
end
@event
func Deny(user : felt):
end
@event
func Join(user : felt, wad : Uint256):
end
@event
func Exit(user : felt, wad : Uint256):
end
@event
func Cage():
end


# VatLike public vat;   // CDP Engine
# bytes32 public ilk;   // Collateral Type
# GemLike public gem;
# uint    public dec;
# uint    public live;  // Active Flag
@storage_var
func _vat() -> (res : felt):
end
@storage_var
func _ilk() -> (res : felt):
end
@storage_var
func _gem() -> (res : felt):
end
@storage_var
func _dec() -> (res : felt):
end
@storage_var
func _live() -> (res : felt):
end


# // --- Auth ---
# mapping (address => uint) public wards;
# function rely(address usr) external auth {
#     wards[usr] = 1;
#     emit Rely(usr);
# }
# function deny(address usr) external auth {
#     wards[usr] = 0;
#     emit Deny(usr);
# }
# modifier auth {
#     require(wards[msg.sender] == 1, "GemJoin/not-authorized");
#     _;
# }
@storage_var
func _wards(user : felt) -> (res : felt):
end
@external
func rely{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }(user : felt):
    auth()
    _wards.write(user, 1)
    Rely.emit(user)
    return ()
end
@external
func deny{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }(user : felt):
    auth()
    _wards.write(user, 0)
    Deny.emit(user)
    return ()
end
func auth{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }():
    let (caller) = get_caller_address()
    let (ward) = _wards.read(caller)
    with_attr error_message("GemJoin/not-authorized"):
      assert ward = 1
    end
    return ()
end


# function cage() external auth {
#     live = 0;
#     emit Cage();
# }
@external
func cage{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }():
    auth()
    _live.write(0)
    Cage.emit()
    return ()
end


# constructor(address vat_, bytes32 ilk_, address gem_) public {
#     wards[msg.sender] = 1;
#     live = 1;
#     vat = VatLike(vat_);
#     ilk = ilk_;
#     gem = GemLike(gem_);
#     dec = gem.decimals();
#     emit Rely(msg.sender);
# }
@constructor
func constructor{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }(
    vat : felt,
    ilk : felt,
    gem : felt,
    ward : felt
  ):
    _wards.write(ward, 1)
    _live.write(1)
    _vat.write(vat)
    _ilk.write(ilk)
    _gem.write(gem)
    let (dec) = IGem.decimals(gem)
    _dec.write(dec)
    Rely.emit(ward)
    return ()
end

@external
func join{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }(user : felt, wad : Uint256):
    alloc_locals

    let (caller) = get_caller_address()
    let (contract_address) = get_contract_address()

    # require(live == 1, "GemJoin/not-live");
    let (live) = _live.read()
    with_attr error_message("GemJoin/not-live"):
      assert live = 1
    end

    # require(int(wad) >= 0, "GemJoin/overflow");
    local syscall_ptr: felt* = syscall_ptr
    let (res) = uint256_le(Uint256(0, 0), wad)
    with_attr error_message("GemJoin/overflow"):
      assert res = 1
    end

    # vat.slip(ilk, usr, int(wad));
    let (vat) = _vat.read()
    let (gem) = _gem.read()
    let (ilk) = _ilk.read()
    IVat.slip(vat, ilk, user, wad)

    # require(gem.transferFrom(msg.sender, address(this), wad), "GemJoin/failed-transfer");
    with_attr error_message("GemJoin/failed-transfer"):
      let (res,) = IGem.transferFrom(gem, caller, contract_address, wad)
      assert res = 1
    end

    # emit Join(usr, wad);
    Join.emit(user, wad)

    return ()
end

@external
func exit{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }(user : felt, wad : Uint256):
    # require(wad <= 2 ** 255, "GemJoin/overflow");
    with_attr error_message("GemJoin/overflow"):
      assert_uint256_le(wad, Uint256(low=2**128-1, high=2**128-1))
    end

    # vat.slip(ilk, msg.sender, -int(wad)); TODO
    let (vat) = _vat.read()
    let (ilk) = _ilk.read()
    let (gem) = _gem.read()
    IVat.slip(vat, ilk, user, wad)

    # require(gem.transfer(usr, wad), "GemJoin/failed-transfer");
    with_attr error_message("GemJoin/failed-transfer"):
      let (res,) = IGem.transfer(gem, user, wad)
      assert res = 1
    end

    # emit Exit(usr, wad);
    Exit.emit(user, wad)

    return ()
end

func assert_uint256_le{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }(a : Uint256, b : Uint256):
    let (is_le) = uint256_le(a, b)
    assert is_le = 1
    return()
end
