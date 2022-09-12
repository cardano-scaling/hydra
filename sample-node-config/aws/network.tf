variable "cidr_vpc" {
  description = "CIDR block for VPC"
  default     = "10.1.0.0/16"
}

variable "cidr_subnet" {
  description = "CIDR block for subnet"
  default     = "10.1.0.0/20"
}

resource "aws_vpc" "hydra-vpc" {
  cidr_block           = var.cidr_vpc
  enable_dns_hostnames = true
  enable_dns_support   = true
}

resource "aws_subnet" "hydra-subnet" {
  vpc_id     = aws_vpc.hydra-vpc.id
  cidr_block = var.cidr_subnet
}

resource "aws_security_group" "hydra-sg" {
  name   = "hydra-sg"
  vpc_id = aws_vpc.hydra-vpc.id

  ingress {
    from_port = 22
    to_port   = 22
    protocol  = "tcp"
    cidr_blocks = [
      "0.0.0.0/0"
    ]
  }

  ingress {
    from_port = 5001
    to_port   = 5001
    protocol  = "tcp"
    cidr_blocks = [
      "0.0.0.0/0"
    ]
  }

  ingress {
    from_port = 80
    to_port   = 80
    protocol  = "tcp"
    cidr_blocks = [
      "0.0.0.0/0"
    ]
  }

  ingress {
    from_port = 443
    to_port   = 443
    protocol  = "tcp"
    cidr_blocks = [
      "0.0.0.0/0"
    ]
  }

  # Terraform removes the default rule, so we re-add it.
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_internet_gateway" "hydra-ig" {
  vpc_id = aws_vpc.hydra-vpc.id
}

resource "aws_route_table" "hydra-rt" {
  vpc_id = aws_vpc.hydra-vpc.id
  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.hydra-ig.id
  }
}

resource "aws_route_table_association" "hydra-rta" {
  subnet_id      = aws_subnet.hydra-subnet.id
  route_table_id = aws_route_table.hydra-rt.id
}
