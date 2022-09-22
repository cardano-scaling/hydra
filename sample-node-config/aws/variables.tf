variable "personal_config" {
  type = object({
    ami            = string
    profile        = string
    region         = string
    instance_type  = string
    key_name       = string
    private_key    = string
    gh_account     = string
    tag            = string
    log_group_name = string
  })
  default = {
    ami            = "ami-04f862d90d8e4ebfc" // Ubuntu 20.10 https://cloud-images.ubuntu.com/locator/ec2/
    profile        = "personal"              // aws-profile configured
    region         = "eu-west-3"             // Paris
    instance_type  = "t2.micro"              // t2.micro is available in the AWS free tier
    key_name       = "dev-personal"          // name of key-pair created
    private_key    = "./env/personal.pem"
    gh_account     = "ffakenz"
    tag            = "Hydraw"
    log_group_name = "instance_logs"
  }
}
