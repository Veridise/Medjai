%lang starknet

from starkware.cairo.common.cairo_builtins import (HashBuiltin, BitwiseBuiltin)
from starkware.cairo.common.uint256 import Uint256
from contracts.vat import (init, hope, rely, gem, _dai, _debt, dai)
from contracts.safe_math import (mul, add)

const ONE = 10 ** 27

@external
func mint{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr,
    bitwise_ptr : BitwiseBuiltin*
  }(user : felt, wad : Uint256):
    let (dai) = _dai.read(user)
    let (res) = mul(wad, Uint256(ONE, 0))
    let (res2) = add(dai, res)
    _dai.write(user, res2)
    let (debt) = _debt.read()
    let (res3) = add(debt, res)
    _debt.write(res3)

    return ()
end
