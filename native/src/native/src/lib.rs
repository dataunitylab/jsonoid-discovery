use std::collections::hash_map::RandomState;

use jni::JNIEnv;
use jni::objects::JObject;
use jni::sys::{jint, jlong};

use hyperloglogplus::{HyperLogLog, HyperLogLogPlus};

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_IntHyperLogLog_init(
  _env: JNIEnv,
  _object: JObject
) -> jlong {
  let hllp: HyperLogLogPlus<i32, _> = HyperLogLogPlus::new(16, RandomState::new()).unwrap();
  Box::into_raw(Box::new(hllp)) as i64
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_IntHyperLogLog_add(
  env: JNIEnv,
  object: JObject,
  value: jint,
) {
  let hllp_ptr =  env.get_field(object, "nativeHLL", "J").unwrap().j().unwrap() as *mut HyperLogLogPlus<i32, RandomState>;
  let hllp_ref = unsafe { &mut *hllp_ptr };
  hllp_ref.insert(&value);
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_IntHyperLogLog_count(
  env: JNIEnv,
  object: JObject
) -> jlong {
  let hllp_ptr =  env.get_field(object, "nativeHLL", "J").unwrap().j().unwrap() as *mut HyperLogLogPlus<i32, RandomState>;
  let hllp_ref = unsafe { &mut *hllp_ptr };
  hllp_ref.count() as i64
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_IntHyperLogLog_free(
  env: JNIEnv,
  object: JObject
) {
  unsafe {
      let hllp_raw = Box::from_raw(env.get_field(object, "nativeHLL", "J").unwrap().j().unwrap() as *mut HyperLogLogPlus<i32, RandomState>);
      drop(*hllp_raw);
  }
}
