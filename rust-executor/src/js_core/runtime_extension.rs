/* 
#[op]
async fn op_read_file(path: String) -> Result<String, AnyError> {
    let contents = tokio::fs::read_to_string(path).await?;
    Ok(contents)
}

#[op]
async fn op_write_file(path: String, contents: String) -> Result<(), AnyError> {
    tokio::fs::write(path, contents).await?;
    Ok(())
}

#[op]
fn op_remove_file(path: String) -> Result<(), AnyError> {
    std::fs::remove_file(path)?;
    Ok(())
}


let _runtime_extension = Extension::builder("runtime")
//.js(include_js_files!(
//    executor "../../executor/lib/bundle.js",
//))
//.esm(include_js_files!(
//    executor "../../executor/lib/bundle.js",
//))
.js(include_js_files!(
    runtime "runtime.js",
)) 
.ops(vec![
    //op_read_file::decl(),
    //op_write_file::decl(),
    //op_remove_file::decl(),
])
.build();

*/