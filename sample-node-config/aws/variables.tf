variable "profile" {
  description = "aws-profile configured by the user in ~/.aws/credentials"
  type        = string
}

variable "key_name" {
  description = "name of key-pair created by the user in aws ec2 > 'Network & Security'"
  type        = string
}

variable "gh_account" {
  description = "user github account"
  type        = string
}

variable "cw_agent" {
  description = "user github account"
  type        = string
}

variable "private_key" {
  description = "the path of your *.pem file relative to this"
  type        = string
}

variable "tag" {
  description = "marker to be used to identify your instance"
  type        = string
}

variable "log_group_name" {
  description = "log group name to be used in cloudwatch to store all your container logs"
  type        = string
}

variable "iam_role" {
  description = "define your instance role"
  type        = string
}

variable "iam_instance_profile" {
  description = "define your instance profile for your role"
  type        = string
}

variable "ami" {
  description = "Amazon Machine Images Id"
  type        = string
  default     = "ami-04f862d90d8e4ebfc" // Ubuntu 20.10 https://cloud-images.ubuntu.com/locator/ec2/
}

variable "region" {
  description = "AWS region to be used to deploy all resources"
  type        = string
  default     = "eu-west-3" // Paris
}

variable "instance_type" {
  description = "define your instance profile for your role"
  type        = string
  default     = "t2.micro" // t2.micro is available in the AWS free tier
}
