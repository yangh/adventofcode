use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::collections::HashMap;

fn main() {
    let rgbm: HashMap<&str, usize> = HashMap::from([("red", 0), ("green", 1), ("blue", 2)]);
    let rgbt: HashMap<&str, i32> = HashMap::from([("red", 12), ("green", 13), ("blue", 14)]);

    // File hosts.txt must exist in the current path
    if let Ok(lines) = read_lines("./input.txt") {
        let mut gamesv = Vec::new();

        // Consumes the iterator, returns an (Optional) String
        for line in lines {
            if let Ok(ip) = line {
                //println!("{}", ip);
                let mut gamev: Vec<Vec<i32>> = Vec::new();
                let v: Vec<&str> = ip.split(":").collect();
                let games: Vec<&str> = v[1].split(";").collect();
                for g in games {
                    //println!("game -{}-", g);
                    let mut roundv: Vec<i32> = Vec::new();
                    roundv.resize(3, 0);
                    let rgbs:Vec<&str> = g.split(",").collect();
                    for color in rgbs {
                        //println!("round -{}-", c);
                        let vc:Vec<&str> = color.strip_prefix(" ").unwrap().split(" ").collect();
                        //println!("num: -{}-", vc[0]);
                        let n: i32 = vc[0].parse::<i32>().unwrap();
                        roundv[rgbm[vc[1]]] = n;
                    }
                    gamev.push(roundv);
                }
                gamesv.push(gamev);
            }
        }

        let mut xv = Vec::new();
        let mut idx = 1;
        gamesv.iter().for_each(|gamev| {
            let count = gamev.iter().fold(0, |total, roundv| {
                let valid_c = rgbt.iter().fold(0, |acc, color| {
                    if *color.1 >= roundv[rgbm[color.0]] {
                        acc + 1
                    } else {
                        acc
                    }
                });
                //println!("valid {}", valid_c);
                if valid_c == 3 {
                    total + 1
                } else {
                    total
                }
            });
            //println!("Count {}", count);

            if count == gamev.iter().count() {
                //println!("Found {}", idx);
                xv.push(idx);
            }
            idx += 1;
        });

        //for v in xv { println!("{} ", v); }

        let sum = xv.iter().fold(0, |acc, v| acc + v);
        println!("Sum of valid game {}", sum);
        if sum == 2377 {
            println!("PASS");
        } else {
            println!("FAILED");
        }

        //PART2
        let mut xv = Vec::new();
        gamesv.iter().for_each(|gamev| {
            let mut roundv: Vec<i32> = Vec::new();
            roundv.resize(3, 0);
            gamev.iter().for_each(|v| {
                for idx in vec![0, 1, 2] {
                    if v[idx] > roundv[idx] {
                        roundv[idx] = v[idx];
                    }
                }
            });
            xv.push(roundv);
        });

        let sum = xv.iter().fold(0, |acc, v| acc + (v[0] * v[1] *v[2]));
        println!("Sum of game powers {}", sum);
    }
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
