# important links:
# cloudwatch pricing details: https://aws.amazon.com/cloudwatch/pricing/
# how to configure agents: https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Agent-Configuration-File-Details.html

variable "log_retention_days" {
  default = "1"
}

locals {
  log_group_name = "${var.key_name}-${var.env}_instance_logs"
}

# store CloudAgent config inside SSM parameter
resource "aws_ssm_parameter" "cw_agent" {
  description = "Cloudwatch agent config to configure custom log"
  name        = "${var.key_name}-${var.env}_cloudwatch-agent_config"
  type        = "String"
  value = templatefile("cw_agent_config.tftpl", {
    log_group_name = local.log_group_name
  })
}

# create log group to be used by CloudAgent config
resource "aws_cloudwatch_log_group" "docker_log_group" {
  name              = local.log_group_name
  retention_in_days = var.log_retention_days
  # tags - (Optional): to distinguish between different log groups
  tags = {
    Environment = "${var.key_name}-${var.env}"
    Application = "${var.key_name}-${var.env}"
  }
}

resource "aws_cloudwatch_log_stream" "cloudwatch_agent" {
  name           = "cloudwatch_agent"
  log_group_name = aws_cloudwatch_log_group.docker_log_group.name
}

resource "aws_cloudwatch_log_stream" "syslog" {
  name           = "syslog"
  log_group_name = aws_cloudwatch_log_group.docker_log_group.name
}

resource "aws_cloudwatch_log_stream" "cardano_node" {
  name           = "cardano_node"
  log_group_name = aws_cloudwatch_log_group.docker_log_group.name
}

resource "aws_cloudwatch_log_stream" "hydra_node" {
  name           = "hydra_node"
  log_group_name = aws_cloudwatch_log_group.docker_log_group.name
}
