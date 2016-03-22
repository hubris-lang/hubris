use iron::prelude::*;
use iron::status;
use router::Router;
use urlencoded::UrlEncodedQuery;

fn handler(req: &mut Request) -> IronResult<Response> {
    // match req.get_ref::<UrlEncodedQuery>() {
    //     Err(ref e) => {
    //         let err_msg = format!("{:?}", e);
    //         Ok(Response::with((status::Ok, &err_msg[..])))
    //     },
    //     Ok(ref hashmap) => {
    //         println!("Parsed GET request query string:\n {:?}", hashmap);
    //         Ok(Response::with((status::Ok, "he")))
    //     }
    // }
    panic!("fix me, something in Iron broke")
}

pub fn run() {
    let mut router = Router::new();  // Alternative syntax:
    router.get("/check", handler);  //                      get "/:query" => handler);

    Iron::new(router).http("127.0.0.1:3000").unwrap();
}
