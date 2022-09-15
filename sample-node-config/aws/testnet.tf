// based on: https://serverfault.com/questions/1084705/unable-to-ssh-into-a-terraform-created-ec2-instance

terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.28"
    }
  }
}

provider "aws" {
  profile = "personal"  // aws-profile configured
  region  = "eu-west-3" // Paris
}

output "instance_ip" {
  value = aws_instance.hydra.public_dns
}

resource "aws_instance" "hydra" {

  ami           = var.personal_config.ami
  instance_type = var.personal_config.instance_type
  key_name      = var.personal_config.key_name

  security_groups             = ["${aws_security_group.hydra-sg.id}"]
  subnet_id                   = aws_subnet.hydra-subnet.id
  associate_public_ip_address = true

  provisioner "file" {
    source      = "scripts/configure-instance.sh"
    destination = "/home/ubuntu/configure-instance.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "scripts/configure-testnet.sh"
    destination = "/home/ubuntu/configure-testnet.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "scripts/fuel-testnet.sh"
    destination = "/home/ubuntu/fuel-testnet.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "scripts/create-marker-utxo.sh"
    destination = "/home/ubuntu/create-marker-utxo.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "scripts/hydraw-up.sh"
    destination = "/home/ubuntu/hydraw-up.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "scripts/hydraw-down.sh"
    destination = "/home/ubuntu/hydraw-down.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "scripts/run-tui.sh"
    destination = "/home/ubuntu/run-tui.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "credentials/cardano-key.sk"
    destination = "/home/ubuntu/cardano-key.sk"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "credentials/arnaud-cardano.vk"
    destination = "/home/ubuntu/arnaud-cardano.vk"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "credentials/hydra-key.sk"
    destination = "/home/ubuntu/hydra-key.sk"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "credentials/arnaud-hydra.vk"
    destination = "/home/ubuntu/arnaud-hydra.vk"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "docker/prometheus.yml"
    destination = "/home/ubuntu/prometheus.yml"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "docker/promtail-config.yml"
    destination = "/home/ubuntu/promtail-config.yml"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/devnet/genesis-shelley.json"
    destination = "/home/ubuntu/genesis-shelley.json"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/protocol-parameters.json"
    destination = "/home/ubuntu/protocol-parameters.json"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "docker/docker-compose.yaml"
    destination = "/home/ubuntu/docker-compose.yaml"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/ubuntu/configure-instance.sh",
      "/home/ubuntu/configure-instance.sh"
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/ubuntu/configure-testnet.sh",
      "/home/ubuntu/configure-testnet.sh ${var.personal_config.gh_account}"
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(var.personal_config.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  tags = {
    Name = "Hydraw"
  }
}
