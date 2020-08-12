sudo apt install ansible git
git clone https://github.com/Purrrrrr/settings.git
cd settings/ansible/
./prequisites.sh 
echo "Try running: ansible-playbook -i inventories/localhost --ask-vault-pass -K playbook.yml"

