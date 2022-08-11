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
from starkware.cairo.common.uint256 import (Uint256, split_64)
from contracts.safe_math import (mul)

@contract_interface
namespace IDAI:
  func burn(from_address : felt, value : Uint256):
  end

  func mint(to_address : felt, value : Uint256):
  end
end

@contract_interface
namespace IVat:
  func move(from_address : felt, to_address : felt, rad : Uint256):
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

#  // --- Auth ---
#  mapping (address => uint) public wards;
#  function rely(address usr) external auth {
#      wards[usr] = 1;
#      emit Rely(usr);
#  }
#  function deny(address usr) external auth {
#      wards[usr] = 0;
#      emit Deny(usr);
#  }
#  modifier auth {
#      require(wards[msg.sender] == 1, "DaiJoin/not-authorized");
#      _;
#  }
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

# VatLike public vat;      // CDP Engine
# DSTokenLike public dai;  // Stablecoin Token
# uint    public live;     // Active Flag
@storage_var
func _vat() -> (res : felt):
end
@storage_var
func _dai() -> (res : felt):
end
@storage_var
func _live() -> (res : felt):
end

# uint constant ONE = 10 ** 27;
const RAY = 10**27

func live{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }():
    let (live) = _live.read()
    with_attr error_message("GemJoin/not-live"):
      assert live = 1
    end
    return ()
end


@external
func cage{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }():
    auth()
    # live = 0;
    _live.write(0)
    # emit Cage();
    Cage.emit()
    return ()
end

# constructor(address vat_, address dai_) public {
#     wards[msg.sender] = 1;
#     live = 1;
#     vat = VatLike(vat_);
#     dai = DSTokenLike(dai_);
# }
@constructor
func constructor{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }(
    vat : felt,
    dai : felt,
    ward: felt
  ):
    _wards.write(ward, 1)
    _live.write(1)
    _vat.write(vat)
    _dai.write(dai)

    return ()
end


# function mul(uint x, uint y) internal pure returns (uint z) {
#     require(y == 0 || (z = x * y) / y == x);
# }
@external
func join{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
  }(user : felt, wad : Uint256):
    let (contract_address) = get_contract_address()
    let (caller) = get_caller_address()

    let (vat) = _vat.read()
    let (dai) = _dai.read()

    # vat.move(address(this), usr, mul(ONE, wad));
    let (value) = mul(Uint256(RAY, 0), wad)
    IVat.move(vat, contract_address, user, value)

    # dai.burn(msg.sender, wad);
    IDAI.burn(dai, caller, wad)

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
    # require(live == 1, "DaiJoin/not-live");
    live()

    let (vat) = _vat.read()
    let (dai) = _dai.read()

    let (contract_address) = get_contract_address()
    let (caller) = get_caller_address()

    # vat.move(msg.sender, address(this), mul(ONE, wad));
    let (value) = mul(Uint256(RAY, 0), wad)
    IVat.move(vat, caller, contract_address, value)

    # dai.mint(usr, wad);
    IDAI.mint(dai, user, wad)

    # emit Exit(usr, wad);
    Exit.emit(user, wad)

    return ()
end
