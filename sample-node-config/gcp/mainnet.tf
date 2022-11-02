variable "use_snapshot" {
  type = number
  default = 0
}

resource "google_compute_disk" "node-mainnet-image" {
  count = var.use_snapshot == 1 ? 0 : 1
  name  = "node-mainnet-disk-image"
  type  = "pd-ssd"
  zone  = "europe-west1-b"
  size  = 200
  image = "iog-hydra-1665828710"
  labels = {
    environment = "mainnet"
  }
}

resource "google_compute_disk" "node-mainnet-snapshot" {
  count = var.use_snapshot == 1 ? 1 : 0
  name  = "node-mainnet-disk-snapshot"
  type  = "pd-ssd"
  zone  = "europe-west1-b"
  size  = 200
  snapshot = var.use_snapshot == 1 ? "iog-hydra-node-mainnet-snapshot" : ""
  labels = {
    environment = "mainnet"
  }
}

resource "google_compute_instance" "hydra-mainnet" {
  name         = "hydra-mainnet-1"

  # https://cloud.google.com/compute/docs/compute-optimized-machines
  machine_type = "c2-standard-4"
  allow_stopping_for_update = true

  tags = [ "hydra", "mainnet" ]

  metadata = {
    sshKeys = file("ssh_keys")
  }

  boot_disk {
    source = var.use_snapshot == 1 ? google_compute_disk.node-mainnet-snapshot[0].self_link : google_compute_disk.node-mainnet-image[0].self_link
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
