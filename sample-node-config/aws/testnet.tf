provider "aws" {
  profile = var.profile
  region  = var.region
}

locals {
  userdata = templatefile("scripts/user_data.sh", {
    ssm_cloudwatch_config = aws_ssm_parameter.cw_agent.name
  })
  private_key = "./aws/${var.key_name}.pem"
  tag         = "${var.key_name}-${var.env}"
}

output "instance_dns" {
  value = aws_instance.this.public_dns
}

resource "aws_eip" "this" {
  vpc      = true

  lifecycle {
    # `terraform apply` will not replace this resource.
    ignore_changes = all
  }

  tags = {
    Name        = "${var.key_name}"
  }
}

resource "aws_eip_association" "this" {
  instance_id   = aws_instance.this.id
  allocation_id = aws_eip.this.id
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
      "mkdir ~/scripts",
      "mkdir ~/credentials",
      "mkdir ~/config",
      "mkdir ~/docker"
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
    destination = "./scripts/"

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
    destination = "./credentials/"

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
    destination = "./docker/"

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
    source      = "config/"
    destination = "./config/"

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
      "mv ~/scripts/.bash_profile ~",
      "find ~/scripts -type f -iname \"*.sh\" -exec chmod +x {} +"
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
      "export ENV=${var.env}",
      "~/scripts/configure-instance.sh"
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
      "export KEY_NAME=${var.key_name}",
      "export ENV=${var.env}",
      "~/scripts/configure-testnet.sh"
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
