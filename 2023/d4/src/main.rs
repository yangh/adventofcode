use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    // File hosts.txt must exist in the current path
    if let Ok(lines) = read_lines("./input.txt") {
        let mut gamesv: Vec<_> = Vec::new();

        // Consumes the iterator, returns an (Optional) String
        for line in lines {
            if let Ok(ip) = line {
                //println!("{}", ip);
                let strs: Vec<_> = ip.split(": ").collect();
                let cards: Vec<_> = strs[1].split("| ").collect();
                gamesv.push((str_to_nums(cards[0]), str_to_nums(cards[1])));
            }
        }

        // Part1
        let mut xv = Vec::new();

        gamesv.iter().for_each(|(card1, card2)| {
            let mut point = 0;
            card1.iter().for_each(|c1| {
                card2.iter().for_each(|c2| {
                    if c1 == c2 {
                        if point == 0 {
                            point = 1;
                        } else {
                            point *= 2;
                        }
                    }
                })
            });
            xv.push(point);
        });
        let sum = xv.iter().fold(0, |acc, v| acc + v);

        println!("Sum of valid game {}", sum);
        adv(sum, 25231);

        // PART2
        let mut cards = Vec::<u32>::new();
        let mut idx: usize = 0;
        cards.resize(gamesv.len(), 0);

        gamesv.iter().for_each(|(card1, card2)| {
            let mut point = 0;
            card1.iter().for_each(|c1| {
                card2.iter().for_each(|c2| {
                    if c1 == c2 {
                        point += 1;
                    }
                })
            });

            println!("{point} points at card {idx}, copies {}", cards[idx]);
            cards[idx] += 1;
            if point > 0 {
                for cid in (idx + 1)..(idx + 1 + point) {
                    println!("  Add {} for card idx {cid}", cards[idx]);
                    cards[cid] += cards[idx];
                }
                idx += 1;
            } else {
                if cards[idx] > 0 {
                    idx += 1;
                } else {
                    println!("No more cards at {idx}");
                }
            }
        });

        let sum = cards.iter().fold(0, |acc, v| acc + v);
        println!("Sum of game powers {}", sum);
        adv(sum, 9721255);
    }
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn str_to_nums(str: &str) -> Vec<i32> {
    let card1: Vec<_> = str.split(" ").collect();
    card1.iter().map(|c| {
        if c.is_empty() {
            None
        } else {
            Some(c.parse::<i32>().unwrap()) }
        }).flatten().collect()
}

fn adv (ret: u32, n: u32) {
        if ret != n {
            println!("ERROR: expected {}, got {}", n, ret);
        } else {
            println!("PASS");
        }
}
