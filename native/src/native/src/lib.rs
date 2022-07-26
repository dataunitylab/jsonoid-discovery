use std::collections::hash_map::RandomState;

use jni::objects::{JObject, JString};
use jni::sys::{jlong, jstring};
use jni::JNIEnv;

use hyperloglogplus::{HyperLogLog, HyperLogLogPlus};

fn get_hllp_ref<'a, T: std::hash::Hash>(env: JNIEnv, object: JObject) -> &'a mut HyperLogLogPlus<T, RandomState> {
    let hllp_ptr = env
        .get_field(object, "nativeHLL", "J")
        .unwrap()
        .j()
        .unwrap() as *mut HyperLogLogPlus<T, RandomState>;
    unsafe { &mut *hllp_ptr }
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_IntHyperLogLog_init(
    _env: JNIEnv,
    _object: JObject,
) -> jlong {
    let hllp: HyperLogLogPlus<i64, _> = HyperLogLogPlus::new(16, RandomState::new()).unwrap();
    Box::into_raw(Box::new(hllp)) as i64
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_IntHyperLogLog_add(
    env: JNIEnv,
    object: JObject,
    value: jlong,
) {
    let hllp_ref = get_hllp_ref::<i64>(env, object);
    hllp_ref.insert(&value);
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_IntHyperLogLog_count(
    env: JNIEnv,
    object: JObject,
) -> jlong {
    let hllp_ref = get_hllp_ref::<i64>(env, object);
    hllp_ref.count() as i64
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_IntHyperLogLog_merge(
    env: JNIEnv,
    object: JObject,
    other: JObject,
) {
    let hllp_ref = get_hllp_ref::<i64>(env, object);
    let hllp_ref2 = get_hllp_ref::<i64>(env, other);
    hllp_ref.merge(hllp_ref2).ok();
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_IntHyperLogLog_free(
    env: JNIEnv,
    object: JObject,
) {
    unsafe {
        let hllp_raw = Box::from_raw(
            env.get_field(object, "nativeHLL", "J")
                .unwrap()
                .j()
                .unwrap() as *mut HyperLogLogPlus<i32, RandomState>,
        );
        drop(*hllp_raw);
    }
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_StringHyperLogLog_init(
    _env: JNIEnv,
    _object: JObject,
) -> jlong {
    let hllp: HyperLogLogPlus<String, _> = HyperLogLogPlus::new(16, RandomState::new()).unwrap();
    Box::into_raw(Box::new(hllp)) as i64
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_StringHyperLogLog_add(
    env: JNIEnv,
    object: JObject,
    value: jstring,
) {
    let hllp_ref = get_hllp_ref::<String>(env, object);
    let jni_str = env.get_string(JString::from(value)).unwrap();
    let rust_str = jni_str.to_str().unwrap();
    hllp_ref.insert(rust_str);
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_StringHyperLogLog_count(
    env: JNIEnv,
    object: JObject,
) -> jlong {
    let hllp_ref = get_hllp_ref::<String>(env, object);
    hllp_ref.count() as i64
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_StringHyperLogLog_merge(
    env: JNIEnv,
    object: JObject,
    other: JObject,
) {
    let hllp_ref = get_hllp_ref::<String>(env, object);
    let hllp_ref2 = get_hllp_ref::<String>(env, other);
    hllp_ref.merge(hllp_ref2).ok();
}

#[no_mangle]
pub extern "system" fn Java_edu_rit_cs_mmior_jsonoid_discovery_utils_StringHyperLogLog_free(
    env: JNIEnv,
    object: JObject,
) {
    unsafe {
        let hllp_raw = Box::from_raw(
            env.get_field(object, "nativeHLL", "J")
                .unwrap()
                .j()
                .unwrap() as *mut HyperLogLogPlus<String, RandomState>,
        );
        drop(*hllp_raw);
    }
}
