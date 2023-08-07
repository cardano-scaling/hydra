variable "profile" {
  description = "The AWS profile configured by the user in ~/.aws/credentials"
  type        = string
}

variable "key_name" {
  description = "The name of key-pair created by the user in aws ec2 > 'Network & Security'. This is also used as your INSTANCE-ID (tag)."
  type        = string
}

variable "env" {
  description = "Environment variable for the remote-exec script."
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
  description = "The instance profile for your role"
  type        = string
  default     = "t2.xlarge" // t2.micro is available in the AWS free tier
}
