resource "google_compute_instance" "hydra-testnet" {
  name         = "hydra-testnet-1"

  # https://cloud.google.com/compute/docs/compute-optimized-machines
  machine_type = "c2-standard-4"
  allow_stopping_for_update = true

  tags = [ "hydra", "testnet" ]

  metadata = {
    sshKeys = file("ssh_keys")
  }

  boot_disk {
    initialize_params {
      size  = 50
      image = "iog-hydra-1637229888"
    }
  }

  network_interface {
    network       = "default"
    access_config {
      nat_ip = google_compute_address.hydra-testnet-address.address
    }
  }

  provisioner "file" {
    source      = "scripts/configure-testnet.sh"
    destination = "/home/curry/configure-testnet.sh"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "file" {
    source      = "scripts/fuel-testnet.sh"
    destination = "/home/curry/fuel-testnet.sh"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "file" {
    source      = "arnaud.sk"
    destination = "/home/curry/arnaud.sk"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "file" {
    source      = "arnaud-hydra.sk"
    destination = "/home/curry/arnaud-hydra.sk"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "file" {
    source      = "prometheus.yml"
    destination = "/home/curry/prometheus.yml"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/devnet/genesis-shelley.json"
    destination = "/home/curry/genesis-shelley.json"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/protocol-parameters.json"
    destination = "/home/curry/protocol-parameters.json"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "file" {
    source      = "docker-compose.yaml"
    destination = "/home/curry/docker-compose.yaml"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "file" {
    source      = "promtail-config.yml"
    destination = "/home/curry/promtail-config.yml"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/curry/configure-testnet.sh",
      "/home/curry/configure-testnet.sh"
    ]

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

}

resource "google_compute_address" "hydra-testnet-address" {
  name = "hydra-testnet-address"
}

output "hydra-testnet-ip" {
  value = google_compute_address.hydra-testnet-address.address
}

output "project" {
  value = google_compute_instance.hydra-testnet.project
}
