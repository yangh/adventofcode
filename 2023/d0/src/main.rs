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
                println!("{}", ip);
                gamesv.push(ip);
            }
        }

        // Part1
        let mut xv = Vec::new();
        xv.push(0);
        let sum = xv.iter().fold(0, |acc, v| acc + v);

        println!("Sum of valid game {}", sum);
        adv(sum, 2377);

        // PART2
        let mut xv = Vec::new();
        xv.push(0);
        let sum = xv.iter().fold(0, |acc, v| acc + v);
        println!("Sum of game powers {}", sum);
        adv(sum, 71200);
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
