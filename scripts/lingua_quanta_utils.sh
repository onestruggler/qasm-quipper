# Unary function to exit with a non-zero exit code and an error message.
error_exit() {
    >&2 echo "$1"
    exit 1
}

# Unary function to ensure temporary directories are configured correctly.
setup_tmpdir() {
    # If no temporary directory is provided, then a temporar directory is generated.
    file=$1
    if [ -z "${file}" ]; then file=$(mktemp -d); fi

    # If the temporary directory does not exist, then it is created.
    if [ ! -d "${file}" ]; then mkdir "${file}"; fi

    # Returns the result.
    echo "${file}"
}
