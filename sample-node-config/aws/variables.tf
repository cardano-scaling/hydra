variable "personal_config" {
  type = object({
    ami                  = string
    profile              = string
    region               = string
    instance_type        = string
    key_name             = string
    private_key          = string
    gh_account           = string
    tag                  = string
    log_group_name       = string
    cw_agent             = string
    iam_role             = string
    iam_instance_profile = string
  })
  default = {
    ami                  = "ami-04f862d90d8e4ebfc" // Ubuntu 20.10 https://cloud-images.ubuntu.com/locator/ec2/
    profile              = "iog"                   // aws-profile configured
    region               = "eu-west-3"             // Paris
    instance_type        = "t2.micro"              // t2.micro is available in the AWS free tier
    key_name             = "franco"                // name of key-pair created
    private_key          = "./env/franco.pem"
    gh_account           = "ffakenz"
    tag                  = "hydraw-franco"
    log_group_name       = "franco_instance_logs"
    cw_agent             = "franco_cloudwatch-agent_config"
    iam_role             = "EC2-Role-Franco"
    iam_instance_profile = "EC2-Profile-Franco"
  }
}
