use reqwest::blocking::Client;
use serde_json::json;
use serde::Deserialize;

#[derive(Deserialize)]
struct Ip {
    message: String,
}

fn main() {
    let client = Client::new();
    let res = fetch_todo(client).unwrap();
    // println!("{:?}", res.text())
    // println!("{:?}", res.json::<Ip>())
    let ip = res.json::<Ip>().unwrap();
    println!("{:?}", ip.message);
}

fn fetch_todo(client: Client) -> Result<reqwest::blocking::Response, reqwest::Error> {
    let url = "http://haruhi/api/balances/1";

    client.get(url).send()
}

fn create_todo(client: Client) -> Result<reqwest::blocking::Response, reqwest::Error> {
    let url = "http://haruhi/api/balances";

    let body = json!({
        "title": "foo",
        "body": "bar",
        "userId": 1
    });

    client.post(url).json(&body).send()
}
