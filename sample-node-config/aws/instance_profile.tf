locals {
  role_policy_arns = [
    "arn:aws:iam::aws:policy/service-role/AmazonEC2RoleforSSM",
    "arn:aws:iam::aws:policy/CloudWatchAgentServerPolicy"
  ]
  iam_role             = "ec2-role-${var.key_name}-${var.env}"
  iam_instance_profile = "ec2-profile-${var.key_name}-${var.env}"
}

# define our role
resource "aws_iam_role" "this" {
  name = local.iam_role
  path = "/"

  assume_role_policy = jsonencode(
    {
      "Version" : "2012-10-17",
      "Statement" : [
        {
          "Action" : "sts:AssumeRole",
          "Principal" : {
            "Service" : "ec2.amazonaws.com"
          },
          "Effect" : "Allow"
        }
      ]
    }
  )
}

# create embedded policy for our role
resource "aws_iam_role_policy" "this" {
  name = "ec2-inline-policy"
  role = aws_iam_role.this.id
  policy = jsonencode(
    {
      "Version" : "2012-10-17",
      "Statement" : [
        {
          "Effect" : "Allow",
          "Action" : [
            "ssm:GetParameter"
          ],
          "Resource" : "*"
        }
      ]
    }
  )
}

# create EC2 profile
resource "aws_iam_instance_profile" "this" {
  name = local.iam_instance_profile
  role = aws_iam_role.this.name
}

# add two policies to: connect to SSM and CloudWatchAgent
resource "aws_iam_role_policy_attachment" "this" {
  count      = length(local.role_policy_arns)
  role       = aws_iam_role.this.name
  policy_arn = element(local.role_policy_arns, count.index)
}

