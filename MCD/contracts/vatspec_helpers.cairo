%lang starknet

from starkware.cairo.common.cairo_builtins import (HashBuiltin, BitwiseBuiltin)
from starkware.cairo.common.uint256 import (
  Uint256,
  uint256_add,
  uint256_sub,
  uint256_mul,
  uint256_eq,
  uint256_le,
  uint256_lt,
  uint256_check
)
from starkware.starknet.common.syscalls import get_caller_address
from starkware.cairo.common.math import (
  assert_not_equal,
)

from contracts.vat import (
    Ilk, # structs
    # _ghost_ilks_sum,
    _live, _wards, _can, _ilks, _dai, _Line, _debt, _vice, # storage vars
    cage, deny, init, hope, nope, file, file_ilk, move, # methods
)

from contracts.safe_math import (
    _mul, sub, add, 
)

from veridise.cairo.verification import (
    medjai_assume_neq_felt,
    medjai_assert_eq_felt,
    medjai_assert_neq_felt,
    medjai_assert_eq_uint256,
    medjai_assert_le_uint256,
    medjai_assert_lt_uint256,
    medjai_assert_valid_uint256,
    medjai_make_symbolic_felt,
    medjai_make_symbolic_uint256,
)

func ward_not_set{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (
    res : felt
):
    alloc_locals

    let (local caller) = get_caller_address()
    let (ward) = _wards.read(caller)
    if ward == 0:
        return (res=1)
    else:
        return (res=0)
    end
end

func live_not_set{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (
    res : felt
):
    alloc_locals

    let (live) = _live.read()
    if live == 0:
        return (res=1)
    else:
        return (res=0)
    end
end

func file_failure_cond{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    what : felt
) -> (res : felt):
    let (res1) = ward_not_set()
    if res1 == 1:
        return (res=1)
    else:
        let (res2) = live_not_set()
        if res2 == 1:
            return (res=1)
        else:
            if what == 'Line':
                return (res=0)
            else:
                return (res=1)
            end
        end
    end
end

func file_ilk_helper(what : felt) -> (res : felt):
    if what == 'line':
        return (res=1)
    else:
        if what == 'spot':
            return (res=1)
        else:
            if what == 'dust':
                return (res=1)
            else:
                return (res=0)
            end
        end
    end
    return (res=0)
end

func valid_what_ilk(what : felt) -> (res : felt):
    if what == 'line':
        return (res = 1)
    end
    if what == 'spot':
        return (res = 1)
    end
    if what == 'dust':
        return (res = 1)
    end
    return (res = 0)
end

func symbolicUint256() -> (res: Uint256):
    let (r) = medjai_make_symbolic_uint256()
    return (res=r)
end

func verify_le_uint256{range_check_ptr}(a : Uint256, b : Uint256):
    medjai_assert_le_uint256(a, b)
    return ()
end

func verify_lt_uint256{range_check_ptr}(a : Uint256, b : Uint256):
    medjai_assert_lt_uint256(a, b)
    return ()
end

func verify_uint256{range_check_ptr}(a : Uint256):
    medjai_assert_valid_uint256(a)
    return ()
end

func verify_eq_uint256{range_check_ptr}(a : Uint256, b : Uint256):
    medjai_assert_eq_uint256(a, b)
    return()
end

@external
func inv_pre{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(i : felt, u : felt):
    alloc_locals

    # let (gem) = _gem.read(i, u)
    let (dai) = _dai.read(u)
    # let (sin) = _sin.read(u)
    # let (debt) = _debt.read()
    # let (vice) = _vice.read()
    # let (Line) = _Line.read()

    # uint256_check(gem)
    uint256_check(dai)
    # uint256_check(sin)
    # uint256_check(debt)
    # uint256_check(vice)
    # uint256_check(Line)

    return ()
end

@external
func inv_post{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    i : felt, u : felt
):
    alloc_locals

    # let (gem) = _gem.read(i, u)
    let (dai) = _dai.read(u)
    # let (sin) = _sin.read(u)
    # let (debt) = _debt.read()
    # let (vice) = _vice.read()
    # let (Line) = _Line.read()

    # verify_Uint256(gem)
    verify_uint256(dai)
    # verify_Uint256(sin)
    # verify_Uint256(debt)
    # verify_Uint256(vice)
    # verify_Uint256(Line)

    return ()
end

#func _ghost_ilks_sum_update{
#    syscall_ptr : felt*,
#    pedersen_ptr : HashBuiltin*,
#    range_check_ptr,
#    bitwise_ptr : BitwiseBuiltin*
#}(old_val: Ilk, new_val: Ilk):
#    alloc_locals
#
#    let (old_agg) = _ghost_ilks_sum.read()
#    let (old_val_mul) = _mul(old_val.Art, old_val.rate)
#    let (new_val_mul) = _mul(new_val.Art, new_val.rate)
#
#    # NOTE: sub will assert that old_agg >= old_val_mul
#    let (sub_res) = sub(old_agg, old_val_mul)
#    let (new_agg) = add(sub_res, new_val_mul)
#
#    _ghost_ilks_sum.write(new_agg)
#
#    return ()
#end

func eq_of_dai_pre{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr,
    bitwise_ptr : BitwiseBuiltin*
}():
    alloc_locals

    let (debt) = _debt.read()
    let (vice) = _vice.read()
    let ilk_agg : Uint256 = Uint256(0, 0) #_ghost_ilks_sum.read()

    let (sum_res) = add(vice, ilk_agg)

    assert debt = sum_res
    # TODO
    #medjai_assert_eq_uint256(ilk_agg, Uint256(0, 0))

    return ()
end

func eq_of_dai_post{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr,
    bitwise_ptr : BitwiseBuiltin*
}():
    alloc_locals

    let (debt) = _debt.read()
    let (vice) = _vice.read()
    let ilk_agg : Uint256 = Uint256(0, 0) #_ghost_ilks_sum.read()

    let (sum_res) = add(vice, ilk_agg)

    verify debt = sum_res

    return ()
end
