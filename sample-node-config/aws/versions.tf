// based on: https://serverfault.com/questions/1084705/unable-to-ssh-into-a-terraform-created-ec2-instance

terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.28"
    }
  }
}
