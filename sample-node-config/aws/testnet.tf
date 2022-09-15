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
    source      = "scripts/"
    destination = "/home/ubuntu"

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
    source      = "credentials/"
    destination = "/home/ubuntu"

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
    source      = "docker/"
    destination = "/home/ubuntu"

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

  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/ubuntu/configure-instance.sh",
      "chmod +x /home/ubuntu/configure-testnet.sh",
      "chmod +x /home/ubuntu/create-marker-utxo.sh",
      "chmod +x /home/ubuntu/hydraw-up.sh",
      "chmod +x /home/ubuntu/hydraw-down.sh",
      "chmod +x /home/ubuntu/run-tui.sh"
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
