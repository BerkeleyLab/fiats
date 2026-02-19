## Inference using new trained ML model for the app of aerosol emulation 
## by Z.Bai, zhebai@lbl.gov
## This file was copied from https://github.com/zhbai/AerosolML

import torch
import torch.nn as nn

class BranchNet(nn.Module):
    def __init__(self, input_dim, output_dim):
        super(BranchNet, self).__init__()
        self.branch_net = nn.Sequential(
            nn.Linear(input_dim, 256),
            nn.GELU(),
            nn.Linear(256, 384),
            nn.GELU(),
            nn.Linear(384, 128),
            nn.GELU(),
            nn.Linear(128, output_dim))

    def forward(self, x):
        return self.branch_net(x)
        
class TrunkNet(nn.Module):
    def __init__(self, input_dim, output_dim, v):
        super(TrunkNet, self).__init__()
        # self.identity = nn.Parameter(torch.eye(output_dim), requires_grad=False)
        self.trunk_net = nn.Sequential(
            nn.Linear(input_dim, 64),
            nn.GELU(),
            nn.Linear(64, output_dim))
        self.basis = v[:,:output_dim]#.reshape(256,20)
    def forward(self, x):
        batch_size = x.shape[0]
        output = torch.concat((self.basis.unsqueeze(0).repeat(batch_size, 1, 1), self.trunk_net(x).unsqueeze(-1)), dim=2)
        return output #basis, trunk

class MyNet_ADON(nn.Module):
    def __init__(self, branch_net, trunk_net, ymean):
        super(MyNet_ADON, self).__init__()
        self.branch_net = branch_net
        self.trunk_net = trunk_net
        self.ymean = ymean
        
    def forward(self, branch_input, trunk_input):
        branch_output = self.branch_net(branch_input) #[batch_size, output_dim]
        trunk_output = self.trunk_net(trunk_input) 
        out = torch.bmm(trunk_output, branch_output.unsqueeze(-1)).squeeze(-1) + self.ymean #
        return out

class toTensor:
    # def __init__(self, device = 'cpu'):
    #     self.device = torch.device(device)
    def __call__(self, array):
        tensor = torch.tensor(array, dtype=torch.float64)
        return tensor
        
class powTransform:
    def __init__(self, par):
        self.par = par
    def __call__(self, sample):
        sample = odd_pow(sample, self.par)
        return sample

class normalize:
    def __init__(self, mean, std):
        self.mean = mean
        self.std =std
    def __call__(self, sample):
        sample -= self.mean
        sample /= self.std
        return sample

def transform_y(y, mean_y, std_y):
    y *= std_y
    y +=  mean_y
    return y

def odd_pow(input, exponent):
    return input.sign() * input.abs().pow(exponent)

if __name__ == "__main__":
    main()