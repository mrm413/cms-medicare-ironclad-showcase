#!/usr/bin/env python3
"""
Post-processor for Ironclad-generated Rust to fix compile errors on complex
enterprise COBOL (CMS Medicare pricers).
"""

import re
import os
import sys
import glob


def fix_empty_into(content):
    """Replace "".into() with 0i64 in arithmetic contexts."""
    content = re.sub(r'""\s*\.into\(\)\s*as\s+i64', '0i64', content)
    content = re.sub(r'""\s*\.into\(\)\s*as\s+usize', '0usize', content)
    content = re.sub(r'""\s*\.into\(\)', '0i64', content)
    return content


def fix_decimal_as_i64(content):
    """Replace (expr) as i64 with i64::from(expr) where expr involves Decimal operations.
    Runs multiple passes until stable."""
    for _ in range(5):
        prev = content
        lines = content.split('\n')
        result = []
        for line in lines:
            new_line = line
            offset = 0
            while True:
                idx = new_line.find(') as i64', offset)
                if idx == -1:
                    break
                paren_end = idx
                depth = 1
                pos = paren_end - 1
                while pos >= 0 and depth > 0:
                    if new_line[pos] == ')':
                        depth += 1
                    elif new_line[pos] == '(':
                        depth -= 1
                    pos -= 1
                paren_start = pos + 1

                if depth == 0:
                    inner = new_line[paren_start:paren_end + 1]
                    if ('state.' in inner or 'Decimal' in inner or
                        '/' in inner or '*' in inner or '+' in inner or
                        '-' in inner or 'i64::from' in inner):
                        before = new_line[:paren_start]
                        after = new_line[paren_end + len(') as i64'):]
                        new_line = before + 'i64::from(' + inner[1:-1] + ')' + after
                        offset = paren_start + len('i64::from(') + len(inner) - 2 + 1
                    else:
                        offset = idx + 1
                else:
                    offset = idx + 1
            result.append(new_line)
        content = '\n'.join(result)
        if content == prev:
            break
    return content


def fix_packed_decimal_as_usize(content):
    """Fix (expr) as usize where expr might be PackedDecimal.
    Wraps with (i64::from(expr)) as usize — safe for u32, i32, i64, PackedDecimal, Decimal.
    Only applies in subscript contexts (followed by .min() bounds check)."""
    # Pattern: ((state.xxx - N) as usize).min(M)
    # Replace inner expression with i64::from(inner) as usize
    content = re.sub(
        r'\(\((state\.\S+?)\s*-\s*(\S+?)\)\s*as\s+usize\)\.min\(',
        r'((i64::from(\1 - \2)) as usize).min(',
        content
    )
    # Pattern without subtraction: (state.xxx as usize).min(M) — less common
    content = re.sub(
        r'\((state\.\S+?)\s+as\s+usize\)\.min\(',
        r'(i64::from(\1) as usize).min(',
        content
    )
    return content


def fix_zero_length_arrays(content):
    """Change [Type; 0] to [Type; 1] in struct field definitions."""
    content = re.sub(r': \[(\w+); 0\]', r': [\1; 1]', content)
    return content


def fix_missing_hprovstate_fields(content):
    """Add h_cbsa_last_pos and h_ipps_cbsa_last_pos to HProvState struct."""
    if 'pub struct HProvState {' not in content:
        return content
    struct_match = re.search(
        r'pub struct HProvState \{.*?\n\}',
        content, re.DOTALL
    )
    if struct_match and 'h_cbsa_last_pos' in struct_match.group(0):
        return content

    old = 'pub struct HProvState {\n    /// FILLER PIC FILLER\n    pub _filler_1282: FixedString<1>,\n    /// H-MSA-LAST-POS\n    pub h_msa_last_pos: FixedString<1>,\n}'
    new = ('pub struct HProvState {\n'
           '    /// FILLER PIC FILLER\n'
           '    pub _filler_1282: FixedString<1>,\n'
           '    /// H-MSA-LAST-POS\n'
           '    pub h_msa_last_pos: FixedString<1>,\n'
           '    /// H-CBSA-LAST-POS (REDEFINES H-MSA-LAST-POS)\n'
           '    pub h_cbsa_last_pos: FixedString<1>,\n'
           '    /// H-IPPS-CBSA-LAST-POS (REDEFINES H-MSA-LAST-POS)\n'
           '    pub h_ipps_cbsa_last_pos: FixedString<1>,\n'
           '}')
    if old in content:
        content = content.replace(old, new)
        old_display = 'write!(f, "{}{}", self._filler_1282, self.h_msa_last_pos)'
        new_display = 'write!(f, "{}{}{}{}", self._filler_1282, self.h_msa_last_pos, self.h_cbsa_last_pos, self.h_ipps_cbsa_last_pos)'
        content = content.replace(old_display, new_display, 1)
    return content


def fix_u32_i32_comparison(content):
    """Fix u32 <= i32 comparisons by casting i32 side to u32."""
    content = re.sub(
        r'(state\.\S+\.bill_units1)\s*(<=|>=|<|>|==|!=)\s*(state\.\S+\.high_rate_days_left)\b',
        r'\1 \2 \3 as u32',
        content
    )
    return content


def fix_array_format_index(content):
    """Fix format!("{}", array)[index].field → array[index].field."""
    content = re.sub(
        r'format!\("{}", (state\.\w+(?:\.\w+)*)\)\[',
        r'\1[',
        content
    )
    return content


def fix_array_direct_field_access(content):
    """Fix array.field → array[0].field for specific known array fields only.
    Only target fields that are actually typed as arrays in the generated code."""
    # These are OCCURS array fields where the transpiler emits direct field access
    # without a subscript. DO NOT include struct fields like rufl_data_tab.
    array_fields = [
        'wwd_entry', 'com_date_entry', 'bun_date_entry',
        'prov_entries', 'prov_entries2', 'prov_entries3',
        't_cbsa_data', 'c_cbsa_data',
        'rufl_tab',
    ]
    for af in array_fields:
        # Only match when followed by .field_name (word char), not [index]
        pattern = re.compile(r'(\b' + af + r')\.(\w)')
        content = pattern.sub(r'\1[0].\2', content)
    return content


def fix_struct_as_bool(content):
    """Fix 'if struct_field {' → 'if format!("{}", struct_field).trim() != "" {'
    COBOL IF on a group item checks if the field has non-space content."""
    # Pattern: if state.xxx.p_new_fy_begin_date {
    content = re.sub(
        r'if (state\.\S+\.p_new_fy_begin_date)\s*\{',
        r'if format!("{}", \1).trim() != "" {',
        content
    )
    return content


def fix_string_eq_integer(content):
    """Fix &format!("{}", field) == 0 → format!("{}", field).trim() == "0" """
    # Pattern: &format!("{}", expr) == N or != N  (N is integer literal)
    content = re.sub(
        r'&format!\("{}", (state\.[^)]+)\)\s*==\s*(\d+)\b',
        r'format!("{}", \1).trim() == "\2"',
        content
    )
    content = re.sub(
        r'&format!\("{}", (state\.[^)]+)\)\s*!=\s*(\d+)\b',
        r'format!("{}", \1).trim() != "\2"',
        content
    )
    return content


def fix_string_cmp_u32(content):
    """Fix String < u32 comparisons by wrapping u32 in format!.
    Pattern: } < state.xxx.fed_fy_begin_NN where the left side is a String expr."""
    content = re.sub(
        r'(\}\s*)(<=|>=|<|>)\s*(state\.\S+\.fed_fy_begin_\w+)\b',
        r'\1\2 format!("{}", \3)',
        content
    )
    return content


def fix_decimal_pow_arg(content):
    """Fix .pow(Decimal_expr) where pow expects u32.
    Pattern: .pow(((state.xxx - N))) → .pow(i64::from(state.xxx - N) as u32)"""
    # .pow(((state.xxx.field - N)))
    content = re.sub(
        r'\.pow\(\(\((state\.\S+?)\s*-\s*(\d+)\)\)\)',
        r'.pow(i64::from(\1 - \2) as u32)',
        content
    )
    return content


def fix_trimmed_method(content):
    """Fix .trimmed without parentheses → .trimmed()"""
    content = re.sub(r'\.trimmed\b(?!\()', '.trimmed()', content)
    return content


def fix_string_as_u32(content):
    """Fix String as u32 → .parse::<u32>().unwrap_or_default()"""
    content = re.sub(
        r'(\w+(?:\.\w+)*\.trimmed\(\)(?:\.to_string\(\))?)\s+as\s+u32',
        r'\1.parse::<u32>().unwrap_or_default()',
        content
    )
    # Fix { expr } as u32 (block expression returning String cast to u32)
    content = re.sub(
        r'(\})\s+as\s+u32\b',
        r'\1.parse::<u32>().unwrap_or_default()',
        content
    )
    return content


def fix_pow_i64(content):
    """Fix .pow(Ni64) → .pow(Nu32) where N is an integer."""
    content = re.sub(r'\.pow\((\d+)i64\)', r'.pow(\1u32)', content)
    return content


def fix_into_eq(content):
    """Fix == "X".into() → == "X" to avoid ambiguous Into."""
    content = re.sub(r'==\s*"([^"]+)"\.into\(\)', r'== "\1"', content)
    content = re.sub(r'!=\s*"([^"]+)"\.into\(\)', r'!= "\1"', content)
    return content


def fix_empty_if_else(content):
    """Fix 'if !_found { else { body }}' → 'if _found { body }'.
    The transpiler generates malformed if/else where the if body is empty
    and else is placed inside it. Invert the condition and flatten."""
    content = re.sub(
        r'if !_found \{ else \{ ([^}]*)\}\}',
        r'if _found { \1}',
        content
    )
    # General fallback: any remaining { else { → { } else { with brace fixup
    content = re.sub(r'\{ else \{([^}]*)\}\}', r'{ } else {\1}', content)
    return content


def fix_format_ge_empty(content):
    """Fix &format!(...) >= "" patterns."""
    content = re.sub(
        r'&format!\(([^)]+)\)\s*(>=|<=|>|<|==|!=)\s*""',
        r'format!(\1).as_str() \2 ""',
        content
    )
    return content


def fix_file(filepath):
    """Apply all fixes to a single .rs file."""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    original = content
    content = fix_empty_into(content)
    content = fix_decimal_as_i64(content)
    content = fix_packed_decimal_as_usize(content)
    content = fix_zero_length_arrays(content)
    content = fix_missing_hprovstate_fields(content)
    content = fix_u32_i32_comparison(content)
    content = fix_array_format_index(content)
    content = fix_array_direct_field_access(content)
    content = fix_struct_as_bool(content)
    content = fix_string_eq_integer(content)
    content = fix_string_cmp_u32(content)
    content = fix_decimal_pow_arg(content)
    content = fix_trimmed_method(content)
    content = fix_string_as_u32(content)
    content = fix_pow_i64(content)
    content = fix_into_eq(content)
    content = fix_empty_if_else(content)
    content = fix_format_ge_empty(content)

    if content != original:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)
        return True
    return False


def main():
    target = sys.argv[1] if len(sys.argv) > 1 else 'src/bin'
    if os.path.isdir(target):
        files = sorted(glob.glob(os.path.join(target, '*.rs')))
    elif os.path.isfile(target):
        files = [target]
    else:
        print(f"Not found: {target}")
        sys.exit(1)

    fixed = 0
    for f in files:
        if fix_file(f):
            fixed += 1
            print(f"  Fixed: {os.path.basename(f)}")

    print(f"\n{fixed}/{len(files)} files modified")


if __name__ == '__main__':
    main()
