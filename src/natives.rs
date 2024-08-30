use crate::value::Value;

pub fn clock(_: &mut [Value]) -> Value {
    let time = std::time::SystemTime::now();
    let since_the_epoch = time.duration_since(std::time::UNIX_EPOCH).unwrap();
    Value::Number(since_the_epoch.as_secs_f64())
}
