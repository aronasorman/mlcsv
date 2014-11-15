# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.box = "utopic64"
  config.vm.box_url = "https://cloud-images.ubuntu.com/vagrant/utopic/20141030.1/utopic-server-cloudimg-amd64-vagrant-disk1.box"

  config.vm.provider "virtualbox" do |v|
    v.cpus = 2
    v.memory = 3072
  end

  # Ansible provisioning!
  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "playbook.yml"
  end

end
