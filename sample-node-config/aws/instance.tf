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

output "DNS" {
  value = aws_instance.hydra.public_dns
}

resource "aws_instance" "hydra" {

  ami           = "ami-04f862d90d8e4ebfc" // Ubuntu 20.10 https://cloud-images.ubuntu.com/locator/ec2/
  instance_type = "t2.micro"              // t2.micro is available in the AWS free tier
  key_name      = "dev-personal"          // name of key-pair created

  security_groups             = ["${aws_security_group.hydra-sg.id}"]
  subnet_id                   = aws_subnet.hydra-subnet.id
  associate_public_ip_address = true

  provisioner "file" {
    source      = "scripts/configure-testnet.sh"
    destination = "/home/ubuntu/configure-testnet.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("./env/dev-personal.pem")
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "scripts/run-instance.sh"
    destination = "/home/ubuntu/run-instance.sh"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("./env/dev-personal.pem")
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
      private_key = file("./env/dev-personal.pem")
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "credentials/franco-cardano.sk"
    destination = "/home/ubuntu/franco-cardano.sk"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("./env/dev-personal.pem")
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "credentials/franco-hydra.sk"
    destination = "/home/ubuntu/franco-hydra.sk"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("./env/dev-personal.pem")
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "docker/prometheus.yaml"
    destination = "/home/ubuntu/prometheus.yaml"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("./env/dev-personal.pem")
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
      private_key = file("./env/dev-personal.pem")
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
      private_key = file("./env/dev-personal.pem")
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
      private_key = file("./env/dev-personal.pem")
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/ubuntu/configure-testnet.sh",
      "/home/ubuntu/configure-testnet.sh"
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("./env/dev-personal.pem")
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/ubuntu/run-instance.sh",
      "/home/ubuntu/run-instance.sh"
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("./env/dev-personal.pem")
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  tags = {
    Name = "Hydraw"
  }
}
