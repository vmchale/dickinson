# -*- mode: ruby -*-
# vi: set ft=ruby :

# FreeBSD vagrant for builds
Vagrant.configure("2") do |config|
  config.vm.box = "freebsd/FreeBSD-12.1-STABLE"
  # config.disksize.size = '10GB'
  config.vm.synced_folder ".", "/home/vagrant/dickinson"
end
