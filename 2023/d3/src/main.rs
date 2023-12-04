use core::num;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::ops::Range;
use regex::Regex;

fn main() {
    let matrix: Vec<(i32, i32)> = vec![(-1, -1), (0, -1), (1, -1),
                                       (-1,  0),/*0,  0*/ (1,  0),
                                       (-1,  1), (0,  1), (1,  1)];

    // File hosts.txt must exist in the current path
    if let Ok(lines) = read_lines("./input.txt") {
        let mut gamesv = Vec::new();
        let mut bitmap_dot_v: Vec<Vec<i32>> = Vec::new();
        let mut starsv = Vec::new();
        let mut bitmap_digit_v: Vec<Vec<i32>> = Vec::new();
        let mut lidx = 0;

        // Consumes the iterator, returns an (Optional) String
        for line in lines {
            if let Ok(ip) = line {
                //println!("{}", ip);
                let bm: Vec<i32> = ip.chars().into_iter().map(|c| { if c == '.' { 0 } else { 1 }}).collect();
                let re = Regex::new(r"([0-9]+)").unwrap();

                let digits: Vec<_> = re.find_iter(&ip).map(|d| {
                    (String::from(d.as_str()).parse::<u32>().unwrap(), d.start(), d.end(), lidx)
                }).collect();
                //println!("digits {}", digits.len());

                let star_re = Regex::new(r"(\*)").unwrap();
                let stars: Vec<_> = star_re.find_iter(&ip).map(|s| (s.start(), s.end(), lidx)).collect();
                //println!("stars {}", stars.len());

                // For part2
                let bmd: Vec<i32> = ip.chars().into_iter().map(|c| { if c.is_digit(10) { 1 } else { 0 }}).collect();
                bitmap_digit_v.push(bmd);

                gamesv.push(digits);
                bitmap_dot_v.push(bm);
                starsv.push(stars);
                lidx += 1;
            }
        }

        let mut xv = Vec::new();
        let mut idx = 0;
        let game_count = bitmap_dot_v.len();
        let bitmap_len = bitmap_dot_v[0].len();

        gamesv.iter().for_each(|digitv| {
            let mut l0 = 0;
            let l1 = idx;
            let l2 = idx + 1;

            if idx > 0 {
                l0 = idx - 1;
            }

            digitv.iter().for_each(|dv| {
                let mut r = Range { start: dv.1, end: dv.2 };

                if dv.1 > 0 {
                    r.start = r.start - 1;
                }

                if dv.2 < bitmap_len {
                    r.end = r.end + 1;
                }

                // Find out all adjacents
                let adj = r.into_iter().fold(0, |acc, x| {
                    let mut t = 0;
                    if idx > 0 {
                        t += bitmap_dot_v[l0][x];
                    }
                    if x < dv.1 || x >= dv.2 {
                        t += bitmap_dot_v[l1][x];
                    }
                    if idx < (game_count - 1) {
                        t += bitmap_dot_v[l2][x];
                    }
                    acc + t
                });

                // Save the number
                if adj > 0 {
                  xv.push(dv.0);
                }  
            });
            //println!("Found {} num with adj", xv.len());
            idx += 1;
        });

        let sum = xv.iter().fold(0, |acc, v| acc + v);
        println!("Sum of valid game {}", sum);
        adv(sum, 543867);

        //PART2
        let mut xv = Vec::new();
        let mut stars_with_2numv = Vec::new();

        starsv.iter().for_each(|stars| {
            stars.iter().for_each(|star| {
                let mut nums = Vec::new();
                
                // Check the nums around the star
                matrix.iter().for_each(|m| {
                    let mut x = star.0;
                    let mut y = star.2;
                    let dx = m.0;
                    let dy = m.1;
     
                    if x == 0 {
                        if dx > 0 {
                            x += 1;
                        }
                    } if x == (bitmap_len - 1) {
                        if dx < 0 {
                            x -= 1;
                        }
                    } else {
                        if dx < 0 {
                            x -= 1;
                        } else if dx > 0 {
                            x += 1;
                        }
                    }

                    if y == 0 {
                        if dy > 0 {
                            y += 1;
                        }
                    } else if y == (game_count - 1) {
                        if dy < 0 {
                            y -= 1;
                        }
                    } else {
                        if dy < 0 {
                            y -= 1;
                        } else if dy > 0 {
                            y += 1;
                        }
                    }

                    //println!("Check num at {} line {}", x, y);
                    if bitmap_digit_v[y][x] == 1 {
                        nums.push((x, y));
                    }

                });
                
                //println!("Found {} num around star {} {} at line {}", nums.len(), star.0, star.1, star.2);
                /*
                if nums.len() > 0 {
                    nums.iter().for_each(|n| {
                        println!("num at x {} line {}", n.0, n.1);
                    })
                }
                */

                let mut digits = Vec::new();
                nums.iter().for_each(|n| {
                    gamesv[n.1].iter().for_each(|dv| {
                            if n.0 >= dv.1 && n.0 < dv.2 {
                                digits.push(dv);
                            }
                        });
                });

                digits.dedup();

                if digits.len() == 2 {
                    //println!("Found gear nums {} {}", digits[0].0, digits[1].0);
                    stars_with_2numv.push(digits);
                }
            });
        });

        stars_with_2numv.iter().for_each(|s| {
            xv.push(s[0].0 * s[1].0);
        });

        let sum = xv.iter().fold(0, |acc, v| acc + v);
        println!("Sum of game powers {}", sum);
        adv(sum, 79613331);
    }
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
