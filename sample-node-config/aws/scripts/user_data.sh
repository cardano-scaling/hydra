#! /bin/bash -xe
# Configure and execute CloudWatch Agent

# fail if something goes wrong
set -e
 
echo 'printing all CloudWatch Agent logs'
exec > >(tee /var/log/user-data.log|logger -t user-data-extra -s 2>/dev/console) 2>&1
 
# access the instance's SessionManager to check the installation via: `cat /var/log/user-data.log`
echo 'installing Cloudwatch Agent'
wget https://s3.eu-west-3.amazonaws.com/amazoncloudwatch-agent-eu-west-3/ubuntu/amd64/latest/amazon-cloudwatch-agent.deb
dpkg -i -E ./amazon-cloudwatch-agent.deb
 
echo 'execuitng Cloudwatch Agent using config passed by SSM parameter'
/opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl \
-a fetch-config \
-m ec2 \
-c ssm:${ssm_cloudwatch_config} -s

echo 'printing Cloudwatch Agent status'
sudo /opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl -m ec2 -a status
