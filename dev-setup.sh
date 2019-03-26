#/bin/bash

echo "Setting up python virtual environment"
if ! python3 -m venv dev; then
  echo "Failed to set up virtual environment. Is python3-venv installed?"
  echo 
  echo "If you prefer, you can install dependencies globally (not recommended):"
  echo "  pip3 install -r dev-requirements.txt"
  echo
  echo "To install venv on Ubuntu/Debian:"
  echo "  sudo apt-get install python3-venv"
  exit
fi

echo "Installing packages for development"
source dev/bin/activate
pip install -r dev-requirements.txt
deactivate

echo
echo "Setup finished."
echo "To enter the virtual environment, run:"
echo "  source dev/bin/activate"
echo
echo "When you are ready to leave the virtual environment run:"
echo "  deactivate"

