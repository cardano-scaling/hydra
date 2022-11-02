variable "hydra_image_id" {
  type        = string
  description = "The image tag of hydra service to deploy"
}

variable "cardano_image_id" {
  type        = string
  description = "The image tag of cardano service to deploy"
}

resource "null_resource" "hydra-demo" {

  # trigger a deployment of the aggregator whwen the
  # image_id is updated
  triggers = {
    hydra_image_id = var.hydra_image_id
    cardano_image_id = var.cardano_image_id
  }

  connection {
    type = "ssh"
    user = "curry"
    host = google_compute_instance.hydra-demo.network_interface.0.access_config.0.nat_ip
  }

  provisioner "file" {
    source      = "scripts/configure-testnet.sh"
    destination = "/home/curry/configure-testnet.sh"
  }

  provisioner "file" {
    source      = "scripts/fuel-testnet.sh"
    destination = "/home/curry/fuel-testnet.sh"
  }

  provisioner "remote-exec" {
    inline = [
      "[ -d keys ] || mkdir keys"
    ]
  }

  provisioner "file" {
    source      = "keys/"
    destination = "/home/curry/keys"
  }

  provisioner "file" {
    source      = "prometheus.yml"
    destination = "/home/curry/prometheus.yml"
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/devnet/genesis-shelley.json"
    destination = "/home/curry/genesis-shelley.json"
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/devnet/genesis-alonzo.json"
    destination = "/home/curry/genesis-alonzo.json"
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/protocol-parameters.json"
    destination = "/home/curry/protocol-parameters.json"
  }


  provisioner "file" {
    source      = "docker-compose.yaml"
    destination = "/home/curry/docker-compose.yaml"
  }

  provisioner "file" {
    source      = "promtail-config.yml"
    destination = "/home/curry/promtail-config.yml"
  }

  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/curry/configure-testnet.sh",
      "HYDRA_IMAGE_ID=${var.hydra_image_id} CARDANO_IMAGE_ID=${var.cardano_image_id} /home/curry/configure-testnet.sh"
    ]
  }


}
