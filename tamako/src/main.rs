use curl::easy::Easy;
use rustc_serialize::json::Json;

fn main() {
    get("http://haruhi/api/balances");
}

fn get(url: &str) -> () {
    let mut data = Vec::new();
    let mut handle = Easy::new();
    handle.url(url).unwrap();
    {
        let mut transfer = handle.transfer();
        transfer.write_function(|new_data| {
            data.extend_from_slice(new_data);
            Ok(new_data.len())
        }).unwrap();
        transfer.perform().unwrap();
    }
    let r: String = String::from_utf8(data).unwrap();
    println!("{}", r);
    println!("{}", Json::from_str(&r).unwrap().find("message").unwrap());
}
