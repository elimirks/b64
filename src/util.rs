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
