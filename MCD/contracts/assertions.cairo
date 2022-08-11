from starkware.cairo.common.bitwise import bitwise_and
from starkware.cairo.common.cairo_builtins import (HashBuiltin, BitwiseBuiltin)
from starkware.cairo.common.math import assert_not_zero
from starkware.cairo.common.uint256 import (
  Uint256,
  uint256_eq,
  uint256_lt,
  uint256_signed_le,
)

func either(a: felt, b: felt) -> (res: felt):
    if a+ b == 0:
        return (0)
    end
    return (1)
end

func assert_either(a: felt, b: felt):
    if a + b == 0:
        assert 1 = 0
    end
    return ()
end

func both(a: felt, b: felt) -> (res: felt):
    if a + b == 2:
        return (1)
    end
    return (0)
end

func assert_both(a: felt, b: felt):
    assert a + b = 2
    return ()
end


func not_0(a: Uint256) -> (res: felt):
    if a.low + a.high == 0:
        return (0)
    end
    return (1)
end

func assert_not_0(a: Uint256):
    assert_not_zero(a.low + a.high)
    return ()
end

func assert_0(a: Uint256):
    assert a.low + a.high = 0
    return ()
end


func ge{range_check_ptr}(a: Uint256, b: Uint256) -> (res: felt):
    let (lt : felt) = uint256_lt(a, b)
    return (res = 1 - lt)
end


# signed!
func ge_0{range_check_ptr}(a: Uint256) -> (res: felt):
    let (res) = uint256_signed_le(Uint256(low=0, high=0), a)
    return (res)
end

func le{range_check_ptr}(a: Uint256, b: Uint256) -> (res: felt):
    alloc_locals
    let (local lt) = uint256_lt(a, b)
    let (eq) = uint256_eq(a, b)
    if lt + eq == 0:
        return (0)
    else:
        return (1)
    end
end

func assert_le{range_check_ptr}(a: Uint256, b: Uint256) -> ():
    let (is_le) = le(a, b)
    assert is_le = 1
    return ()
end

# signed!
func le_0{range_check_ptr}(a: Uint256) -> (res: felt):
    let (res) = uint256_signed_le(a, Uint256(low=0, high=0))
    return (res)
end

func eq_0(a: Uint256) -> (res: felt):
    if a.low + a.high == 0:
        return (1)
    end
    return (0)
end
