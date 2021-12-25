use std::{fs, io};

// Gets number of logical CPUs on Linux
pub fn logical_cpu_count() -> usize {
    try_logical_cpu_count().unwrap_or(1)
}

fn try_logical_cpu_count() -> Result<usize, io::Error> {
    let mut logical_cpus = 0;
    for entry in fs::read_dir("/sys/devices/system/cpu")? {
        let name_os_str = entry?.file_name();
        if name_os_str.len() > 3 {
            let name = name_os_str.into_string().unwrap();
            let third_char = name.chars().nth(3).unwrap();
            if third_char.is_numeric() && name.starts_with("cpu") {
                logical_cpus += 1;
            }
        }
    }
    Ok(logical_cpus)
}

const DE_BRUIJN_LBS_BIT_POS: [u32; 32] = [
    0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8, 
    31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
];
/// TL;DR Fucking fast algo to find the least significant bit
/// It will find the value in 4 arithmetic operations and a memory lookup.
/// # References
/// http://supertech.csail.mit.edu/papers/debruijn.pdf
pub fn lsb_number(v: u32) -> u32 {
    let index = (v & v.overflowing_neg().0)
        .overflowing_mul(0x077CB531u32).0
        .overflowing_shr(27).0;
    return unsafe {
        *DE_BRUIJN_LBS_BIT_POS.get_unchecked(index as usize)
    };
}
