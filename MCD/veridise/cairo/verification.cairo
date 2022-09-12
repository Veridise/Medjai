from starkware.cairo.common.uint256 import(
     Uint256,
     uint256_le,
     uint256_lt,
     uint256_eq,
)

# Medjai Symbolics

func medjai_make_symbolic_felt() -> (res : felt):
    return (res=0)
end

func medjai_make_symbolic_uint256() -> (res : Uint256):
    let (l) = medjai_make_symbolic_felt()
    let (h) = medjai_make_symbolic_felt()
    return (res=Uint256(low=l, high=h))
end

# End Symbolics

# Medjai Assertions

# Uint256
func medjai_assert_le_uint256{range_check_ptr}(a : Uint256, b : Uint256):
    let (res) = uint256_le(a, b)
    medjai_assert_eq_felt(res, 1)
    return ()
end

func medjai_assert_lt_uint256{range_check_ptr}(a : Uint256, b : Uint256):
    let (res) = uint256_lt(a, b)
    medjai_assert_eq_felt(res, 1)
    return ()
end

func medjai_assert_geq_uint256{range_check_ptr}(a : Uint256, b : Uint256):
    let (res) = uint256_lt(a, b)
    medjai_assert_eq_felt(res, 0)
    return ()
end

func medjai_assert_gt_uint256{range_check_ptr}(a : Uint256, b : Uint256):
    let (res) = uint256_le(a, b)
    medjai_assert_eq_felt(res, 0)
    return ()
end

func medjai_assert_eq_uint256{range_check_ptr}(a : Uint256, b : Uint256):
    let (res) = uint256_eq(a, b)
    medjai_assert_eq_felt(res, 1)
    return ()
end

func medjai_assert_neq_uint256{range_check_ptr}(a : Uint256, b : Uint256):
    let (res) = uint256_eq(a, b)
    medjai_assert_eq_felt(res, 0)
    return ()
end

# Felt
func medjai_assert_le_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assert_lt_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assert_geq_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assert_gt_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assert_eq_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assert_neq_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

# END Assertions

# Medjai Assumptions

func medjai_assume_le_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assume_lt_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assume_geq_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assume_gt_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assume_eq_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end

func medjai_assume_neq_felt{range_check_ptr}(a : felt, b : felt):
    return ()
end
# END Assumptions

# Medjai Validators

func medjai_assert_valid_uint256{range_check_ptr}(a : Uint256):
    medjai_assert_geq_felt(a.low, 0)
    medjai_assert_lt_felt(a.low, 340282366920938463463374607431768211456)
    medjai_assert_geq_felt(a.high, 0)
    medjai_assert_lt_felt(a.high, 340282366920938463463374607431768211456)
    return ()
end

func medjai_assume_valid_uint256{range_check_ptr}(a : Uint256):
    medjai_assume_geq_felt(a.low, 0)
    medjai_assume_lt_felt(a.low, 340282366920938463463374607431768211456)
    medjai_assume_geq_felt(a.high, 0)
    medjai_assume_lt_felt(a.high, 340282366920938463463374607431768211456)
    return ()
end
# End Validators