#/bin/bash

echo "Setting up python virtual environment"
python3 -m venv dev

echo "Installing packages for development"
source dev/bin/activate
pip install -r dev-requirements.txt
deactivate

echo
echo "Setup finished."
echo "Run \`source dev/bin/activate\` to enter virtual environment."
echo "Run \`deactivate\` to leave virtual environment."

