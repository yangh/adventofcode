use std::fs::File;
use std::io::{self, BufRead};
use std::ops::ControlFlow;
use std::path::Path;
use std::cmp;

fn main() {

    // File hosts.txt must exist in the current path
    if let Ok(lines) = read_lines("./input.txt") {
        let mut gamesv: Vec<_> = Vec::new();
        let mut idx: usize = 0;
        let mut maps: Vec<_> = Vec::new();
        let mut seeds: Vec<i64> = Vec::new();

        // Consumes the iterator, returns an (Optional) String
        for line in lines {
            if let Ok(ip) = line {
                println!("{}", ip);
                if ip.len() > 0 {
                    if idx == 0 {
                        seeds = str_to_nums(&ip);
                        println!("Found {} seeds", seeds.len());
                        seeds.iter().for_each(|s| println!("{s}"));
                    } else {
                        let digits = str_to_nums(&ip);
                        if digits.len() == 0 {
                            if maps.len() > 0 {
                                gamesv.push(maps.clone());
                                maps.clear();
                            }
                        } else {
                            maps.push(digits);
                        }
                    }
                    idx += 1;
                }
            }
        }
        gamesv.push(maps.clone());

        gamesv.iter().for_each(|maps| println!("Map has {} clues", maps.len()));

        // Part1
        let mut loc = 0;
        let mut locs = Vec::new();
        let mut next = 0;

        seeds.iter().for_each(|s| {
            //println!("Find loc for seed {s}");
            loc = gamesv.iter().fold(*s, |acc, maps| {
                next = 0;
                for map in maps {
                    if acc >= map[1] && acc < (map[1] + map[2]) {
                        next = map[0] + (acc - map[1]);
                   } 
                }
                //println!("src {acc} dest {next}");

                if next == 0 {
                    acc
                } else {
                    next
                }
            });
            locs.push(loc);
        });

        locs.iter().for_each(|l| println!("Loc {l}"));
        let mut lowest = locs.iter().fold(locs[0], |acc, loc| {
            if acc < *loc {
                acc
            } else {
                *loc
            }
        });
        println!("Lowest loc: {lowest}");

        adv(lowest, 35);

        // PART2
        let mut xv = Vec::new();
        for i in 0..(seeds.len()/2) {
            xv.push((seeds[i*2], seeds[i*2 + 1]));
        }

        loc = 0;
        locs.clear();

        xv.iter().for_each(|(seed, range)| {
            //println!("Find loc for seed {seed} range {range}");
            for s in *seed..(*seed + *range) {
                //println!("Find loc for seed {s}");
                let loc2 = gamesv.iter().fold(s, |acc, maps| {
                    next = 0;
                    for map in maps {
                        if acc >= map[1] && acc < (map[1] + map[2]) {
                            next = map[0] + (acc - map[1]);
                            //ControlFlow::<i32, String>::Break(0);
                       } 
                    }
                    //println!("src {acc} dest {next}");
     
                    if next == 0 {
                        acc
                    } else {
                        next
                    }
                });

                if loc == 0 {
                    loc = loc2;
                } else {
                    loc = cmp::min(loc, loc2);
                }
            }
        });
        lowest = loc;
        println!("Lowest loc: {lowest}");
    }
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn adv (ret: i64, n: i64) {
        if ret != n {
            println!("ERROR: expected {}, got {}", n, ret);
        } else {
            println!("PASS");
        }
}

fn str_to_nums(str: &str) -> Vec<i64> {
    let card1: Vec<_> = str.split(" ").collect();
    card1.iter().map(|c| {
        if c.is_empty() || ! str_is_digital(c) {
            None
        } else {
            Some(c.parse::<i64>().unwrap()) }
        }).flatten().collect()
}

fn str_is_digital(str: &str) -> bool {
    str.chars().into_iter().fold(true, |acc, c| acc && c.is_digit(10))
}