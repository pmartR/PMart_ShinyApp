if (!isTruthy(Sys.which("python"))) {
	message(paste(sep = "\n",
			"Error: no Python install was detected.",
			"Python 3 is required to be installed to set up the virtual environment.",
			"Please check to make sure Python 3 is installed and it is available on your PATH."
	))
} else {
	if (isTruthy(Sys.which("./venv/Scripts/python"))) {
		message(paste(sep = "\n",
			"Warning: An existing virtual environment has been detected.",
			"Are you sure you want to continue? [Y/n] "
		))
		resp <- readline("")
		if (resp != "Y" && resp != "y") {
			return()
		}
	}
	system("python -m venv ./venv")
	system("./venv/Scripts/activate")
	system("./venv/Scripts/python -m pip install -r requirements.txt")
	system("./venv/Scripts/deactivate")
	message("Python virtual environment has been installed.")
}