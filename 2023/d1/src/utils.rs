// Try to implement a Vec::extract_if() function
// https://doc.rust-lang.org/std/vec/struct.Vec.html#method.extract_if
fn vec_extract_if<V, F>(v : V, mut filter: F) -> bool
where V: AsMut<Vec<u32>>, F: FnMut(&mut T) -> bool, {
    let mut idx = 0;
    while idx < v.len() {
        if filter(&mut v[idx]) {
            v.remove(idx);
        } else {
            idx += 1;
        }
    }
    return true;
}
