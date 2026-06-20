cfg_select! {
    test => {
        mod version_check;
        mod signature_check;
    }
    _ => {}
}
