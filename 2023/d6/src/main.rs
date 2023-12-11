use std::fs::File;
use std::io::{self, BufRead};
use std::ops::ControlFlow;
use std::path::Path;
use std::cmp;

fn main() {

    // File hosts.txt must exist in the current path
    if let Ok(mut lines) = read_lines("./input.txt") {
        let mut times: Vec<i64> = Vec::new();
        let mut dists: Vec<i64> = Vec::new();

        let line1 = lines.next().unwrap();
        let line2 = lines.next().unwrap();
        times = str_to_nums(&line1.unwrap());
        dists = str_to_nums(&line2.unwrap());

        let mut races: Vec<_> = Vec::new();
        for i in 0..times.len() {
            races.push((times[i], dists[i]));

        }

        let mut nums: Vec<_> = Vec::new();
        races.iter().for_each(|(t, d)| {
            //println!("t {} vs d {}", t, d);
            let mut idx = 0;
            let r = (0..(t/2)).try_for_each(|i| {
                //println!("i {}, res {} vs dst {}", i, i* (t-i), *d);
                if (i * (t - i)) > *d {
                    println!("Found at {i}");
                    idx = i;
                    return ControlFlow::Break(i)
                }
                ControlFlow::Continue(())
            });
            nums.push(t - (idx * 2) + 1);
        });

        nums.iter().for_each(|r| println!("Race margin {r}"));
        let multi = nums.iter().fold(1,|acc, r| acc * r);
        println!("Multi of margins {}", multi);
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