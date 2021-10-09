
if (!require('faraway')) install.packages('faraway')
library(faraway)

# faraway  ----

# csa : �Ƶ����� �������� ���������� Abused, �ƴ϶�� NotAbused
# ptsd : �ܻ��� ��Ʈ���� ��� 
# cpa : ���������ν� �Ƶ����� ��ü�� �д�

# �Ƶ����� ������ ������ ������ ���Űǰ��� ��ġ�� ������ �м��� ���� 
str(sexab)

tapply(sexab$ptsd, sexab$csa, mean)

tapply(sexab$ptsd, sexab$csa, sd)

tapply(sexab$ptsd, sexab$csa, length)

# cpa : ������
# csa : ��������
# ptsd : ���Ӻ���

sexab.aov <- aov(ptsd ~ cpa + csa, data = sexab)

summary(sexab.aov)



# �������� ������ ������ ���� ������ �ܻ� �� ��Ʈ���� ����� ���� ��� �ľ�
if (!require('effects')) install.packages('effects')
library(effects)

effect('csa', sexab.aov)

# HH package�� �ִ� ancova�� ����Ѵ�
# ancova ----

ancova(ptsd ~ cpa + csa, data = sexab)