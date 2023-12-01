use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::vec::Vec;

fn main() {
    adv(part1(), 52974);
    adv(part2(), 53340);
}

fn part1() -> u32 {
    let mut total = 0;

    if let Ok(lines) = read_lines("src/input") {
        // Consumes the iterator, returns an (Optional) String
        lines.for_each(|line| {
            if let Ok(cal) = line {
                let mut digits = Vec::<u32>::new();

                cal.chars().for_each(|c| {
                    let n: Option<u32> = c.to_digit(10);
                    if let Some(n) = n {
                        digits.push(n);
                    }
                });

                //println!("{}", cal);
                if digits.len() > 0 {
                    total += digits.first().unwrap() * 10 + digits.last().unwrap();
                } else {
                    println!("Warning: no digital found in {}", cal);
                }
            }
        });

        println!("Total {}", total);
    }

    return total;
}

fn part2() -> u32 {
    let mut total = 0;

    if let Ok(lines) = read_lines("src/input") {
        // Consumes the iterator, returns an (Optional) String
        lines.for_each(|line| {
            if let Ok(cal) = line {
                let mut digits = Vec::<u32>::new();

                //println!("{}", cal);
                // Make room for all possible digits
                digits.resize(cal.chars().count(), 0);

                // Find digitals
                let mut idx = 0;
                for c in cal.chars() {
                    let n: Option<u32> = c.to_digit(10);
                    if let Some(n) = n {
                        digits[idx] = n;
                    }
                    idx += 1;
                }

                // Find digital words and save as digital
                let digits_a = vec![(1, "one"),   (2, "two"),   (3, "three"),
                                    (4, "four"),  (5, "five"),  (6, "six"),
                                    (7, "seven"), (8, "eight"), (9, "nine")];
                for da in digits_a {
                    let v: Vec<_> = cal.match_indices(da.1).collect();
                    for m in v {
                        digits[m.0] = da.0;
                    }
                }

                // Remove 0 values
                digits = vec_extract_u32(digits, |x| x == 0);

                if digits.len() > 0 {
                    total += digits.first().unwrap() * 10 + digits.last().unwrap();
                } else {
                    println!("Warning: no digital found in {}", cal);
                }
            }
        });

        println!("Total {}", total);
    }

    return total;
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn adv (ret: u32, n: u32) {
        if ret != n {
            println!("ERROR: expected {}, got {}", n, ret);
        } else {
            println!("PASS");
        }
}

fn vec_extract_u32(mut v : Vec<u32>, filter: fn(u32) -> bool) -> Vec<u32>
{
    let mut idx = 0;

    while idx < v.len() {
        if filter(v[idx]) {
            v.remove(idx);
        } else {
            idx += 1;
        }
    }
    return v;
}
