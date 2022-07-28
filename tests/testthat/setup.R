conn <- platform_connect(
    api_key = Sys.getenv("GOR_API_KEY"),
    project = Sys.getenv("GOR_API_PROJECT"))

if (!grepl("platform", conn$service_root[1], fixed = TRUE)) {
    stop("Tests should only be run on Platform dev - please reset 'GOR_API_KEY'")
}
