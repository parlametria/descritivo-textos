# Descritivo dos conjuntos de textos do parlametria

Para obter e gerar os dados necessários, que são grandes demais para ficar aqui:

Dados de comissões:

1.  Baixe as transcrições coletadas por Matheus Alves [deste gdrive](https://drive.google.com/drive/folders/1kezGSUkHTbW0jLmdOxgnDpeUggChMXPy).
2.  Coloque-as na pasta `data/raw/comissoes-2019`
3.  ./src/transform-raw2ready.R gerará um csv em `data/ready`

Dados de proposições:

1. Baixe os dados de proposições, disponíveis [aqui](https://drive.google.com/drive/folders/1SvDIpYoHOwxpedU8bCZnuavJQsy34jDo?usp=sharing).
2. Coloque-os na pasta `data/ready`

Os notebooks em `reports/` usam esses dados.
