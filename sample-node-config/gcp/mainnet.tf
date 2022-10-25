resource "google_compute_instance" "hydra-mainnet" {
  name         = "hydra-mainnet-1"

  # https://cloud.google.com/compute/docs/compute-optimized-machines
  machine_type = "c2d-standard-2"
  allow_stopping_for_update = true

  tags = [ "hydra", "mainnet" ]

  metadata = {
    sshKeys = file("ssh_keys")
  }

  boot_disk {
    initialize_params {
      size  = 100
      image = "iog-hydra-1665828710"
    }
  }

  network_interface {
    network       = "default"
    access_config {
      nat_ip = google_compute_address.hydra-mainnet-address.address
    }
  }

  connection {
    type = "ssh"
    user = "curry"
    host = self.network_interface.0.access_config.0.nat_ip
  }

  provisioner "file" {
    source      = "scripts/configure-mainnet.sh"
    destination = "/home/curry/configure-mainnet.sh"
  }

  provisioner "file" {
    source      = "prometheus.yml"
    destination = "/home/curry/prometheus.yml"
  }

  # copy network configuration (genesis, topology)
  provisioner "file" {
    source      = "../../hydra-cluster/config/cardano-configurations/network/mainnet"
    destination = "/home/curry/mainnet"
  }

  provisioner "file" {
    source      = "docker-compose-mainnet.yaml"
    destination = "/home/curry/docker-compose.yaml"
  }

  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/curry/configure-mainnet.sh",
      "/home/curry/configure-mainnet.sh"
    ]
  }

}

resource "google_compute_address" "hydra-mainnet-address" {
  name = "hydra-mainnet-address"
}

output "hydra-mainnet-ip" {
  value = google_compute_address.hydra-mainnet-address.address
}

output "project" {
  value = google_compute_instance.hydra-mainnet.project
}
