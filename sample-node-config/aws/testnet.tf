provider "aws" {
  profile = var.profile
  region  = var.region
}

locals {
  userdata = templatefile("scripts/user_data.sh", {
    ssm_cloudwatch_config = aws_ssm_parameter.cw_agent.name
  })
  private_key = "./env/${var.key_name}.pem"
  tag         = "hydraw-${var.key_name}"
}

output "instance_dns" {
  value = aws_instance.this.public_dns
}

resource "aws_eip" "this" {
  instance = aws_instance.this.id
  vpc      = true
}

output "instance_ip" {
  value = aws_eip.this.public_ip
}

resource "aws_instance" "this" {
  ami                         = var.ami
  instance_type               = var.instance_type
  key_name                    = var.key_name
  security_groups             = ["${aws_security_group.this.id}"]
  subnet_id                   = aws_subnet.this.id
  associate_public_ip_address = true

  ebs_block_device {
    device_name = "/dev/sda1"
    volume_size = 200
  }

  provisioner "remote-exec" {
    inline = [
      "mkdir /home/ubuntu/scripts",
      "mkdir /home/ubuntu/credentials",
      "mkdir /home/ubuntu/config"
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(local.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "scripts/"
    destination = "/home/ubuntu/scripts/"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(local.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "credentials/"
    destination = "/home/ubuntu/credentials/"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(local.private_key)
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
      private_key = file(local.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "./config/preview/protocol-parameters.json"
    destination = "/home/ubuntu/config/protocol-parameters.json"

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(local.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "find /home/ubuntu/scripts -type f -iname \"*.sh\" -exec chmod +x {} +"
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(local.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "/home/ubuntu/scripts/configure-instance.sh"
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(local.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "/home/ubuntu/scripts/configure-testnet.sh"
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file(local.private_key)
      timeout     = "2m"
      agent       = false
      host        = self.public_ip
    }
  }

  iam_instance_profile = aws_iam_instance_profile.this.name
  user_data            = local.userdata

  tags = {
    Name = local.tag
  }
}
